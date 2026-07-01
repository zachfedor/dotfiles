# argus — Raspberry Pi 3 Model B Plus, always-on network node (issue 13, slice 2).
# Jobs: Tailscale node + DNS ad-block via AdGuard Home (ADR-0008, replaces the old
# Raspbian Pi-hole). Headless. Renamed from `raspi` (DECIDED 2026-06-29) — Argus
# Panoptes, the never-sleeping guardian.
#
# Arch note: the 3B+ is a 64-bit-capable Cortex-A53 but was running 32-bit Raspbian
# (armv7l). NixOS targets aarch64-linux instead — aarch64 has a populated binary
# cache; armv7l does not (would force slow local/cross builds). Flake output pins
# system = "aarch64-linux".
#
# Deploy (do when physically present, not from mobile):
#   1. Build the SD image on an aarch64-linux builder (or emulated):
#        nix build .#nixosConfigurations.argus.config.system.build.sdImage
#   2. Flash result/sd-image/*.img.zst to the SD card, boot the Pi.
#   3. `sudo tailscale up`, then `nixos-rebuild switch --flake .#argus` for updates.
# On a 1GB Pi, prefer building on hestia/athena + pushing the closure over
# nixos-rebuild-on-device.

{ config, pkgs, lib, modulesPath, ... }:

{
  imports = [
    # Generic aarch64 SD-image: U-Boot + generic-extlinux bootloader, sets up the
    # partition layout and `system.build.sdImage`. Handles Pi 3 boot; no
    # hardware-configuration.nix needed (unlike athena) — the image module owns
    # the filesystem/boot setup.
    "${modulesPath}/installer/sd-card/sd-image-aarch64.nix"
  ];

  networking.hostName = "argus";

  # Redistributable firmware for the Pi's onboard NIC/wifi/bt (bcm/smsc blobs).
  hardware.enableRedistributableFirmware = true;

  # --- networking: static IP, this box is the LAN DNS server so it must be stable.
  # usePredictableInterfaceNames = false pins the onboard USB-ethernet to `eth0`
  # (predictable naming gives Pi USB NICs unstable enuXuY names). VERIFY on first
  # boot with `ip link`; if the wired iface isn't eth0, fix the name here.
  networking.usePredictableInterfaceNames = false;
  networking.interfaces.eth0.ipv4.addresses = [{
    address = "192.168.1.2";
    prefixLength = 24;
  }];
  networking.defaultGateway = "192.168.1.1";
  # The Pi's OWN resolver = public upstreams, NOT its own AdGuard — avoids a
  # chicken-and-egg where AdGuard being down kills DNS for its own updates.
  networking.nameservers = [ "1.1.1.1" "9.9.9.9" ];

  # --- DNS ad-block: AdGuard Home (ADR-0008). openFirewall opens 53 (tcp/udp) +
  # the web UI. First boot: finish setup at http://argus:3000 (create admin user,
  # bind DNS to :53, add blocklists). mutableSettings keeps web-UI edits across
  # rebuilds. Declarative `settings` (incl. a bcrypt `users` entry) can replace the
  # wizard later — left out for now so the first-run wizard isn't locked out.
  services.adguardhome = {
    enable = true;
    openFirewall = true;
    mutableSettings = true;
  };

  # --- Tailscale: always-on tailnet node. `sudo tailscale up` once after install.
  # Fresh NixOS install issues new host keys, which clears the pre-migration
  # `tailscale ssh`/host-key-verification failure noted in issue 13.
  services.tailscale.enable = true;

  # --- ssh: key-only, same as athena. Login user = zach (post-migration the argus
  # ssh matchBlock in home.nix flips from pi@192.168.1.2 to zach@argus).
  services.openssh = {
    enable = true;
    openFirewall = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };
  programs.ssh.startAgent = true;

  # --- SD-card wear mitigations (1GB Pi, flash storage) ---
  # zram: compressed RAM swap instead of swapping to the SD card. Also eases the
  # 1GB memory ceiling during rebuilds.
  zramSwap.enable = true;
  # journald to RAM: stop the systemd journal from grinding the SD card. Volatile
  # storage + a small cap; logs don't persist across reboot (fine for this node).
  services.journald.extraConfig = ''
    Storage=volatile
    RuntimeMaxUse=64M
  '';

  # Terminfo for all terminals (ships the alacritty entry). Fixes the prompt/
  # backspace cursor-math bug over ssh from alacritty (TERM=alacritty missing from
  # the old Raspbian terminfo DB) — the NixOS terminfo DB + shared home.nix zsh
  # config resolve it. Verify post-migration: `echo $TERM` + a backspace test.
  environment.enableAllTerminfo = true;

  # zsh as system login shell; the zsh/zim *config* comes from the shared home.nix
  # passthrough (same pattern as athena/hestia). enableCompletion off — zim runs
  # its own compinit (avoids the double-init warning).
  programs.zsh.enable = true;
  programs.zsh.enableCompletion = false;

  # Passwordless sudo for wheel: argus is a headless, key-only-access appliance —
  # no interactive password is ever set (no console login), so sudo would otherwise
  # be unusable (locked account = can't authenticate). No secret in the repo. First
  # install missed this, requiring a one-time imperative password bootstrap; this
  # makes it declarative going forward.
  security.sudo.wheelNeedsPassword = false;

  users.users.zach = {
    isNormalUser = true;
    description = "Zach Fedor";
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGgHR3HB18TLl6f8kszcUriyRAZxbPmXIJxVXpr+hyWo zachfedor@gmail.com"
    ];
  };

  # Shared home.nix pulls an unfree pkg (ngrok); allow it as athena/hestia do.
  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  # Minimal system CLI for a headless node (rescue/boot contexts before the HM
  # user profile loads). user zach's richer nvim etc. come from home.nix.
  environment.systemPackages = with pkgs; [
    git
    neovim
    wget
  ];

  # First-install release baseline; leave pinned once installed.
  system.stateVersion = "25.11";
}
