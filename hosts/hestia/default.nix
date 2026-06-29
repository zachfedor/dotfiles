# macOS (nix-darwin) system layer for hestia.
#
# Imported by flake.nix `darwinConfigurations.hestia`. `user` and `system` come
# from the flake's specialArgs so host identity is defined once in flake.nix.
# This file holds everything macOS/nix-darwin-specific; the cross-platform user
# config lives in ../../home.nix.
{ pkgs, user, ... }:

{
  # nixpkgs.hostPlatform comes from the `system` arg passed to darwinSystem in
  # flake.nix (athena relies on the same for nixosSystem).

  # Determinate Nix manages the daemon and nix itself; let it.
  # nix-darwin must NOT also manage nix or the two fight.
  nix.enable = false;

  # Required by nix-darwin for user-scoped options (e.g. home-manager).
  system.primaryUser = user;
  users.users.${user}.home = "/Users/${user}";

  # nix-darwin's /etc/zshrc runs `compinit` by default, which fires BEFORE
  # ~/.zshrc → before zim's `completion` module, triggering zim's "completion
  # was already initialized" warning. Zim owns completion (it must run compinit
  # after its modules register definitions), so turn off nix-darwin's copy. We
  # still enable zsh at the system level for the rest of /etc/zshrc (PATH, etc.).
  programs.zsh.enable = true;
  programs.zsh.enableCompletion = false;

  # Baseline; do not bump casually (see nix-darwin docs).
  system.stateVersion = 5;

  # GUI apps installed via nixpkgs land in /nix/store; nix-darwin's built-in
  # `applications` activation script rsyncs real .app bundles (not symlinks) into
  # /Applications/Nix Apps so Spotlight + Dock can see/pin them — but ONLY for
  # apps in environment.systemPackages (its source is `system-applications`).
  # alacritty + emacs30 live in home.nix (per-user), so home-manager only
  # SYMLINKS them into ~/Applications, which Spotlight/Dock ignore (issue 10
  # regression after dropping the brew casks). Listing them here too puts their
  # bundles in system-applications → real copies in /Applications/Nix Apps.
  # Same store paths as home.nix, just a second profile ref (cheap, no rebuild
  # of the pkgs). Pin to the Dock from /Applications/Nix Apps.
  environment.systemPackages = [
    pkgs.alacritty
    pkgs.emacs30
  ];

  # Unfree user packages (issue 10): ngrok, _1password-cli. useGlobalPkgs=true
  # means home.nix uses this host's pkgs, so allowUnfree must be set here (athena
  # sets its own). Matches athena's nixpkgs.config.allowUnfree.
  nixpkgs.config.allowUnfree = true;

  # Tailscale (issue 10): brought under nix-darwin (was an undeclared brew install).
  # Daemon + CLI from nixpkgs, mirrors athena's services.tailscale. CLI-only (no
  # menubar GUI); authenticate once with `sudo tailscale up`.
  services.tailscale.enable = true;

  # --- GUI apps via Homebrew (issue 04e) ---
  # GUI casks are the one area that's legitimately macOS-only; the NixOS host
  # (athena, #05) gets these from nixpkgs separately. nix-darwin drives brew
  # declaratively — it does NOT install brew itself (Determinate/brew did). CLI
  # tools + fonts live in nix, not here. `cleanup = "none"` preserves the
  # nix-like split: casks LISTED here are declaratively managed ("permanent");
  # anything `brew install`'d manually is an ephemeral test that SURVIVES
  # rebuilds (brew is not auto-pruned). Removing a cask from this list does NOT
  # uninstall it — do that by hand (same as the brew→nix migration rule for CLI
  # tools).
  homebrew = {
    enable = true;
    onActivation = {
      # issue 10: don't auto-update/upgrade every rebuild (slow, noisy,
      # non-reproducible). Run `brew update && brew upgrade` by hand when wanted.
      autoUpdate = false;
      cleanup = "none";
      upgrade = false;
    };
    # Emacs moved to nixpkgs `emacs30` in home.nix (ADR-0005) — the d12frosted
    # tap is gone for good (its untrusted-tap + sudo conflict broke rebuilds).
    # Do NOT reintroduce it. Uninstall the old brew by hand: `brew uninstall
    # emacs-plus@30`.
    taps = [ ];
    brews = [
      "zork"            # no nixpkgs package
    ];
    casks = [
      # essentials
      "firefox"
      "brave-browser"   # OSS chromium for testing (cut chrome/edge/opera/opera-gx)
      "1password"
      "slack"
      "discord"
      "telegram"
      # creative
      "gimp"
      "inkscape"
      "vcv-rack"
      # media / learning
      "vlc"
      "calibre"
      "anki"            # nixpkgs anki drags in qtwebengine (no darwin cache) — cask instead
      "obs"
      "transmission"
      "sabnzbd"
      # games / emulation
      "steam"
      "openemu"
      "openmw"
      # macOS utility
      "hammerspoon"
    ];
  };
}
