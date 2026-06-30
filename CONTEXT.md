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

**Per-project devShell**:
A pinned toolchain (language runtime, LSP, linters, DB) declared in a project's
own `flake.nix` and loaded on `cd` via `.envrc` (`use flake`) + nix-direnv. The
home for language-specific and version-specific tooling — the opposite of a
global install. Global packages are reserved for a thin cross-project fallback
(python3, nodejs) + ad-hoc CLIs (gh, jq).
_Avoid_: "version manager" (pyenv/rbenv/n — retired in favour of devShells).

**Vendored app**:
A GUI app installed by its own vendor installer (not Homebrew or Nix) — e.g.
Ableton, Logitech drivers. Out of Nix scope; reinstalled manually and listed in
`docs/new-host.md`, the same boundary as app-internal state (logins, extensions).

**Audit verdict**:
The disposition assigned to each installed tool during the #10 audit, exactly
one of: **declare** (bring under Nix in the right layer), **ephemeral** (leave
the Homebrew install — survives rebuilds per ADR-0003 — but don't reproduce it on
a fresh box), or **cut** (uninstall). Simplify-biased: cut is the default.

**Doom profile**:
An Emacs config selectable at launch via Doom's native profile system
(`doom/profiles.el`, `emacs --profile NAME`). The default "global" profile is the
plain Doom install at the standard `~/.emacs.d`; extra profiles are opt-in and
sandbox their packages. Replaces Chemacs2 (see ADR-0002).
_Avoid_: "Chemacs profile" (Chemacs is removed).

## Hosts

Named machines in the personal fabric (Tailscale-connected). Greek-myth naming.

- **hestia** — macOS, primary daily driver.
- **athena** — NixOS desktop.
- **mnemosyne** — Synology NAS. Always-on; storage + services host.
- **hephaestus** — DigitalOcean droplet.
- **theseus** — iPhone. Syncs to mnemosyne over WebDAV.
- **daedalus** — iPad. Not configured by this repo; connects to mnemosyne over
  WebDAV for sync, like theseus.
- **argus** — Raspberry Pi (renamed from `raspi`, DECIDED 2026-06-29). Always-on
  network node: Tailscale + DNS ad-block via **AdGuard Home** (`services.adguardhome`,
  replacing Pi-hole — native NixOS module over a container). Also the on-LAN
  Wake-on-LAN relay for the Jellyfin escape hatch. Migrating Raspbian → NixOS.
  Argus Panoptes = never-sleeping guardian.

**Media library**:
The third storage class from #06: large binaries (ebooks, audio, video, photos)
that are neither notes nor active note-linked reference, and must NOT ride the
phone-bound Syncthing tree. **Default canonical home = mnemosyne (NAS)** — one
authoritative copy on the always-on node, co-located with the streaming server.
_Distinct from_: the #06 Syncthing **note tree** and `~/resources/*` (PARA
resources, synced, already on iPad). `~/resources/` was abbreviated `ref/` in
older notes — NOT the same as the `~/Documents/ref/` bulk folder below.

Per-directory disposition (split by directory, not one blanket policy):

| Source (`~/Documents/`) | Class | Canonical home | Reaches | Mechanism |
|---|---|---|---|---|
| `ref/gaming` (emulators+ROMs) | athena-class (exception) | **athena** | athena only | NAS Syncthing backup replica |
| `ref/books` (ebooks/PDFs) | media library | **mnemosyne** | desktops + iPad | WebDAV (iPad) + Syncthing folder (desktops) |
| `calibre/` (ebook DB) | media library | with books | desktop Calibre app | synced local (app dislikes net shares) |
| `ref/dnd` (battlemaps/assets) | leave in place | — | — | archive most; cherry-pick into `~/resources/` as needed |
| `Max 9/` (Cycling '74 Max user lib) | Vendored-app state | `~/Documents/Max 9/` | hestia | app-managed, NOT media library |
| video / music | media library | mnemosyne | Jellyfin clients | Jellyfin (in place) |
| photos | media library | mnemosyne | mobile | Synology Photos — decided separately |

Note: `ref/gaming` is the one exception to NAS-canonical — emulators run on athena,
so athena is the source of truth and the NAS holds a Syncthing backup replica.

**Streaming server**:
**Jellyfin** (DECIDED) — OSS media server, the Plex replacement. Serves both video
and music to TV, phone, and browser clients. **Hosted on mnemosyne** (Synology
Container Manager), reading the **Media library** in place — co-located with
canonical storage, no always-on-desktop dependency. Old Plex retired once live.
_Rejected_: mpd / Navidrome (audio-only — can't serve video or a TV app).
_Transcode note_: the DS923+ (Ryzen R1600, no iGPU) has no hardware transcode —
fine for direct-play + the SD DVD-rip library, weak for 4K/HEVC. **Escape hatch**
(not built): if heavy transcoding ever bites, move Jellyfin to athena (AMD GPU),
woken on demand via Wake-on-LAN with argus as the always-on LAN relay.

## Flagged ambiguities

- "declarative" was used to mean both *rewrite every config in Nix* and *manage
  placement declaratively* — resolved: we mean declarative **placement** now
  (Passthrough), with Native translation as opt-in, not a mandate.
- "cross-OS" was initially aspirational (README); resolved: it is now an **active**
  requirement (macOS + NixOS), Windows excluded.
