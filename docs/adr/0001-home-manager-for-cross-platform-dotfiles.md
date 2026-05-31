# Home-manager as the cross-platform config layer

## Context

This repo was macOS-only, configured imperatively by `install.sh` (backup +
symlink). A second machine now runs NixOS (replacing Windows), and the owner
wants user configuration to be declarative, reproducible, and synced across both
machines. The owner is new to Nix.

## Decision

Adopt **Nix home-manager** as the single cross-platform layer for user-level
configuration on both macOS and NixOS, driven by one flake. `install.sh` retires
once home-manager places the configs.

Migrate **incrementally and passthrough-first**: every existing config (`vimrc`,
`zshrc`, `tmux.conf`, etc.) is placed verbatim via `home.file` and stays plain,
directly-editable text. A config is promoted to a **native Nix module** only when
that specific config needs cross-OS branching or install/config coupling — never
as a big-bang rewrite.

nix-darwin (declarative macOS *system* settings) is deferred, not rejected.

## Why

- **Reproducibility + cross-machine sync** are the top values; home-manager is the
  standard tool that delivers both on macOS and NixOS from one source of truth.
- **Passthrough-first** avoids a months-long rewrite of large existing configs
  (the `vimrc` alone is ~22k) and the high abandonment risk that carries for a Nix
  beginner. It yields the cross-platform win on day one and lets Nix be learned
  gradually.
- Declarative is treated as a means to the project's values, not an end — so we
  buy only as much of it as pays off, when it pays off.

## Considered and rejected

- **Keep `install.sh` on macOS, NixOS-native on Linux** — config defined twice, no
  single source of truth; fails the cross-machine-sync value.
- **Full native-module translation up front** — maximal declarativeness, but
  unrealistic effort for a Nix beginner and high abandon risk.
- **GNU stow / plain symlinks** — cross-platform placement, but no declarative
  package management and no NixOS integration.

## Consequences

- Because passthrough decouples config *content* from *placement*, fixing or
  refining a config's content now (under the old layout) is not wasted work — only
  the symlink wiring changes when it moves under home-manager.
