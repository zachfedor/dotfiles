# Basic NixOS config sharing the flake

Status: needs-triage

## What to build

Bring the newly installed NixOS machine onto the same flake as macOS. Native
NixOS config for the system layer; home-manager for user config, reusing the
passthrough modules so shared dotfiles render identically on both machines.

## Acceptance criteria

- [ ] `flake.nix` exposes a `nixosConfigurations` entry for the NixOS machine
- [ ] System rebuilds with `nixos-rebuild switch --flake`
- [ ] home-manager user config applies on NixOS, reusing the shared passthrough modules
- [ ] At least one shared config (e.g. zshrc, vimrc) verified identical on macOS and NixOS

## Blocked by

- #03 home-manager flake tracer
