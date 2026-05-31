# Dotfiles

Personal configuration for a single user across multiple machines. Migrating
from an imperative, macOS-only symlink setup toward a declarative, cross-platform
one built on Nix.

## Goals & values

In priority order:

1. **Reproducibility** — a new machine reaches a known-good state from this repo,
   not from undocumented manual steps.
2. **Cross-machine sync** — macOS and NixOS share one source of truth for user
   configuration; the same edit applies everywhere it's relevant.
3. **Tidiness** — one obvious place per concern; no dead or duplicated config.
4. **Legibility** — a future reader (including future-me) can reason about what a
   change does and why, without running it.

Declarative is the means, not the goal: chosen because it serves the four values
above. We adopt it incrementally, not all at once.

## Platforms

- **macOS** — primary daily driver. Declarative *user* config via home-manager;
  system-level settings remain partly manual (nix-darwin is possible later, not
  committed).
- **NixOS** — newly installed, replacing a Windows machine. Native NixOS config
  for system; home-manager for user config.
- Windows — being migrated away from; not a target.

## Language

**home-manager**:
The cross-platform layer that owns user-level configuration on both macOS and
NixOS. Single source of truth for dotfile placement and user packages.

**Flake**:
The pinned, reproducible entry point describing both machines' configurations.

**Passthrough config**:
A dotfile that home-manager places verbatim from this repo (e.g.
`home.file."...".source = ./vimrc`). Content stays plain text, edited directly.
The default state for existing configs.
_Avoid_: "symlinked file" (describes mechanism, not intent).

**Native module**:
A config rewritten in the Nix DSL (e.g. `programs.zsh.shellAliases`). Couples a
tool's install and config, enables cross-OS branching. The opt-in target for a
config only when that coupling or branching pays off.

**Incremental translation**:
The migration strategy: every config starts as Passthrough; individual configs
become Native modules one at a time, on demand — never as a big-bang rewrite.

## Relationships

- The **Flake** declares one home-manager configuration per machine.
- Every existing config begins as a **Passthrough config**; **Incremental
  translation** promotes select ones to **Native modules**.
- **home-manager** supersedes `install.sh` as the mechanism that places configs.

## Example dialogue

> **Dev:** "We're going declarative — do I rewrite `vimrc` (22k) as a Nix module?"
> **Owner:** "No. It stays a **Passthrough config**. home-manager just places it.
> We only promote it to a **Native module** if we need it to differ between macOS
> and NixOS, or to bundle plugin installs with it."

## Flagged ambiguities

- "declarative" was used to mean both *rewrite every config in Nix* and *manage
  placement declaratively* — resolved: we mean declarative **placement** now
  (Passthrough), with Native translation as opt-in, not a mandate.
- "cross-OS" was initially aspirational (README); resolved: it is now an **active**
  requirement (macOS + NixOS), Windows excluded.
