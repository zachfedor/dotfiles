# Basic NixOS config sharing the flake

Status: triaged (grilled — ready to build)

## What to build

Bring the custom-built x86_64 desktop **athena** (NixOS, already installed and
booting GNOME) onto the same flake as macOS host **hestia**. Native NixOS config
for the system layer; home-manager (as a NixOS module) for user config, reusing
the shared `home.nix` so dotfiles render identically on both machines.

## Decisions (grilled)

- **Host:** `athena`, `x86_64-linux`, AMD GPU. Already installed + booting GNOME.
  (`hephaestus` is a separate managed server, not in this flake.)
- **Graphical stack:** keep **GNOME** for "basic" (#05). A minimal WM (**hyprland**,
  for hammerspoon-style window-management keybind parity) is wanted later but is a
  **separate new issue**, not #08 (#08 = Flavours/ricing/theming). See carry-out.
- **home.nix factoring:** single shared file with inline
  `pkgs.stdenv.isDarwin`/`isLinux` guards (style A) — platform surface is tiny
  (~4 spots). Split modules deferred until Linux-only surface grows. Also replace
  hardcoded `dotfiles = "/Users/zach/.dotfiles"` with
  `config.home.homeDirectory + "/.dotfiles"`.
- **flake.nix:** `hosts/<host>/` dirs (`hosts/hestia/default.nix`,
  `hosts/athena/{default.nix,hardware-configuration.nix}`); **two explicit output
  blocks** (`darwinConfigurations.hestia` + `nixosConfigurations.athena`), each
  with its own `system`. No `mkHost` helper yet (earns its keep at 3+ hosts). Kill
  the global `let system`.
- **athena system config:** **adopt** the installer's existing
  `/etc/nixos/{configuration,hardware-configuration}.nix` as the tracer base
  (known-good bootloader/fs/networking/locale/GNOME) — relocate verbatim first,
  then layer features. Do **not** bump `system.stateVersion` (keep installer's).
- **HM on NixOS:** as a module (`home-manager.nixosModules.home-manager`),
  mirroring the darwin module — single rebuild command, symmetry with mac.
- **Emacs:** `emacs30-pgtk` from nixpkgs in `home.packages` (`isLinux`-guarded;
  mac keeps `emacs-plus@30` brew). Doom stays **hand-managed** on both (clone
  doomemacs → symlink `~/.emacs.d` → `doom sync` → `doom env`); NOT nix-managed /
  no emacs-overlay (preserves ADR-0002 out-of-store Doom workflow).
- **GUI apps:** system module owns them (mirrors mac `homebrew` casks-in-system).
  `environment.systemPackages`: firefox, vlc, calibre. `steam` via
  `programs.steam.enable` (FHS/udev/32-bit). `nixpkgs.config.allowUnfree = true`
  (slack/discord/steam). **Dropped (macOS-only):** hammerspoon (→ future hyprland),
  openemu. Fonts stay in `home.packages` (already cross-platform) + need
  `fonts.fontconfig.enable` in HM on Linux.
- **User account (NixOS declares from scratch):** `users.users.zach` =
  `isNormalUser`, `extraGroups = [ wheel networkmanager audio video ]`,
  `shell = pkgs.zsh`; `programs.zsh.enable = true` system-side (valid login shell).
  HM keeps owning the zsh *dotfiles*. Password manual (`passwd`/`hashedPassword`),
  **never plaintext in the flake**.
- **zimfw → nixpkgs on BOTH OSes** (style B): finishes the brew→nix migration
  ([[brew-to-nix-migration-rule]]); `brew uninstall zimfw` on mac. Passthrough
  zshrc can't interpolate a store path, so **HM exports an env var** (e.g.
  `ZIM_FW_INIT`) pointing at the nix store; zshrc sources `$ZIM_FW_INIT`, the
  `uname` Darwin/Linux case collapses to one path-free line. (Hard blocker — the
  current `Linux)` branch sources from linuxbrew, absent on NixOS.)
- **nix management:** NixOS manages its own nix normally (no Determinate here →
  no `nix.enable = false`). Bake `nix.settings.experimental-features =
  [ "nix-command" "flakes" ]` into athena's module.

## Bootstrap (athena, one-time)

```sh
# git is NOT permanently installed — ephemeral shell, then it comes from home.packages
nix-shell -p git --run 'git clone <repo-url> ~/.dotfiles'

# first rebuild needs the flakes feature flag inline (stock NixOS has it off)
sudo nixos-rebuild switch --flake ~/.dotfiles#athena \
  --extra-experimental-features 'nix-command flakes'
# (experimental-features baked into athena's module thereafter)

# Doom (hand-managed, both OSes): clone doomemacs → symlink ~/.emacs.d → doom sync → doom env
```

Future rebuilds: `sudo nixos-rebuild switch --flake ~/.dotfiles#athena`

## Slices (each a clean, separately-verifiable rebuild)

- **5a — refactor, zero behavior change (mac-verifiable today).** Lift inline darwin
  system module → `hosts/hestia/default.nix`; per-host `system`; `isDarwin` guards +
  `config.home.homeDirectory` for `dotfiles`; zimfw → nixpkgs-on-both via HM env var.
  **Regression gate:** `darwin-rebuild switch --flake ~/.dotfiles#hestia` clean, mac
  unchanged. (Do this BEFORE touching athena — don't entangle "refactor broke mac"
  with "athena works.")
- **5b — athena tracer (relocation only).** `nix-shell -p git` clone; pull
  `/etc/nixos/{configuration,hardware-configuration}.nix` → `hosts/athena/`; add
  `nixosConfigurations.athena` + flakes flag; first rebuild reproduces **current
  GNOME desktop, zero feature changes.**
- **5c — user + HM on athena.** Declare account (groups + zsh login shell); wire
  `home-manager.nixosModules.home-manager` + shared `home.nix`;
  `fonts.fontconfig.enable`. **Acceptance #4 gate:** log in, shared zshrc/zimfw
  prompt renders identical to mac.
- **5d — Emacs/Doom on athena.** `emacs30-pgtk`; hand-bootstrap Doom (clone →
  symlink → `doom sync` → `doom env`). Primary-editor parity.
- **5e — GUI apps + system polish.** `environment.systemPackages`
  (firefox/vlc/calibre); `programs.steam`; `allowUnfree`; AMD `hardware.graphics`.
  Drop hammerspoon/openemu.

## Acceptance criteria

- [ ] `flake.nix` exposes `nixosConfigurations.athena`; `nix flake check` passes
- [ ] System rebuilds with `nixos-rebuild switch --flake ~/.dotfiles#athena`
- [ ] home-manager user config applies on NixOS (as a NixOS module), reusing the
      shared passthrough `home.nix`
- [ ] Shared config verified identical mac↔athena — **zshrc + zimfw prompt**
      (exercises passthrough + new zimfw env-var path) + a store-passthrough
      spot-check (e.g. `~/.gitconfig`)
- [ ] hestia mac rebuild stays clean after the refactor (5a regression gate)

## Blocked by

- #03 home-manager flake tracer (done)

## Carry-outs

- **NEW ISSUE (triage):** minimal WM **hyprland** on athena with hammerspoon-parity
  window-management keybindings. Distinct from #08 (theming/ricing) — this is WM +
  bindings, not colors. Likely sequences after #08 or alongside.
- `UseKeychain` (home.nix ssh `extraConfig`) is macOS-only — must be `isDarwin`-
  guarded as part of 5a (already flagged in 4c + home.nix:163).
