# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "athena"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable Nix Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Tailscale (issue 06) — joins the tailnet so off-LAN Syncthing can reach the
  # always-on mnemosyne node (and later the iphone's WebDAV access). Authenticate
  # once after switching: `sudo tailscale up`.
  services.tailscale.enable = true;

  # Enable OpenSSH
  services.openssh = {
    enable = true;
    openFirewall = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Run an ssh-agent for the login session. macOS (hestia) always has one via
  # launchd + keychain, but GNOME dropped gnome-keyring's ssh-agent, so without
  # this athena has no agent and `git push` over ssh re-prompts for the key
  # passphrase on every use. With this + home.nix's `addKeysToAgent = "yes"`, the
  # key auto-adds on first use and stays cached for the rest of the session.
  programs.ssh.startAgent = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zach = {
    isNormalUser = true;
    description = "Zach Fedor";
    extraGroups = [ "networkmanager" "wheel" "audio" "video" "docker" ];
    # zsh is the login shell; the zsh/zim *config* comes from home-manager
    # (shared home.nix passthrough). programs.zsh.enable below registers it in
    # /etc/shells so it's a valid login shell. (issue 05c)
    shell = pkgs.zsh;
    openssh = {
      authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGgHR3HB18TLl6f8kszcUriyRAZxbPmXIJxVXpr+hyWo zachfedor@gmail.com"
      ];
    };
    packages = with pkgs; [
    #  thunderbird
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;
  programs.steam.enable = true;

  # 1Password desktop app + CLI integration (issue 10 — NixOS parity with
  # hestia's `1password` cask + the shared `_1password-cli` in home.nix).
  # The shared CLI binary alone can't do desktop-app/biometric unlock; these
  # two NixOS-only modules wire it up:
  #   programs._1password       — installs `op` as a setuid wrapper in
  #     /run/wrappers/bin (group `onepassword-cli`) so the CLI can verify the
  #     running desktop app. /run/wrappers/bin is ahead of the home-manager
  #     profile on PATH, so this `op` shadows home.nix's `_1password-cli` on
  #     athena (hestia keeps using the plain CLI — these modules are Linux-only).
  #   programs._1password-gui   — the GUI app; polkitPolicyOwners grants `zach`
  #     the polkit action for system-auth unlock + the app's CLI-integration toggle.
  # Both are unfree (covered by nixpkgs.config.allowUnfree below). After
  # switching, open the app → Settings → Developer → enable "Integrate with
  # 1Password CLI" (and "Unlock using system authentication" for biometrics).
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "zach" ];
  };

  # GPU graphics stack, explicit (issue 05e). NOTE: redundant while Steam is on —
  # programs.steam already enables hardware.graphics + enable32Bit. Kept explicit
  # so GPU accel (Emacs-pgtk, video decode, future hyprland) survives if Steam is
  # ever disabled. Remove if you'd rather not duplicate. (hardware.graphics is the
  # 25.11 rename of hardware.opengl.)
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # zsh as a system-registered login shell (config via home-manager). zim owns
  # completion (it runs compinit after its modules load), so disable NixOS's
  # /etc/zshrc compinit to avoid the "completion was already initialized"
  # double-init warning — same fix as hestia's nix-darwin config (4d). (issue 05c)
  programs.zsh.enable = true;
  programs.zsh.enableCompletion = false;

  # Containers (issue 10 / ADR-0006): native Docker daemon. The `docker` +
  # `docker-compose` CLIs come from the shared home.nix; on macOS colima backs
  # them with a VM, here the daemon is native (no VM). User is in the docker group.
  virtualisation.docker.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # System-level CLI: available to root/boot/rescue and before the HM user
  # profile loads. neovim stays here as the editor for those contexts — for user
  # zach, home-manager's *configured* nvim (issue 04b) still wins because
  # useUserPackages puts /etc/profiles/per-user/zach/bin ahead of
  # /run/current-system/sw/bin in PATH. (issue 05c)
  environment.systemPackages = with pkgs; [
    git
    neovim
    wget
    # GUI apps (issue 05e): the NixOS equivalents of hestia's mac casks
    # (firefox + steam are enabled via programs.* above; openemu/hammerspoon are
    # macOS-only and dropped). slack/discord are unfree (allowUnfree set below).
    vlc
    calibre
    slack
    discord
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?

}
