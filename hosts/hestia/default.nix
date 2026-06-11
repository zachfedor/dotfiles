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
      autoUpdate = true;
      cleanup = "none";
      upgrade = true;
    };
    taps = [ "d12frosted/emacs-plus" ];
    brews = [
      "emacs-plus@30"   # from-source Emacs w/ native-comp (Doom); see issue 01
      "zork"            # no nixpkgs package
    ];
    casks = [
      "firefox"
      "slack"
      "discord"
      "vlc"
      "calibre"
      "steam"
      "openemu"
      "hammerspoon"
    ];
  };
}
