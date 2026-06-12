# Org notes: sync + open on both machines (tracer)

Status: triaged (grilled — ready to build)

## What to build

The demoable tracer for the Org-mode goal: stand up Syncthing for the org files
and have home-manager place the org config on both machines so the same notes open
on macOS (hestia) and NixOS (athena). Portability baked in from the foundation.

## Decisions (grilled)

- **Sync mechanism: Syncthing** (P2P, no cloud, declarative on the nix hosts).
  Rejected: git (manual commit/pull friction + concurrent-edit conflicts; history
  not worth it here), cloud dir (Dropbox dropped in 4e; iCloud can't reach NixOS).
- **Topology:** 3-peer mesh — hestia + athena + **mnemosyne (Synology NAS) as the
  always-on canonical node**, so the two desktops stay in sync even when only one
  is on (athena isn't always on). Off-LAN traffic rides **Tailscale**.
- **org-directory → `~/notes`** (was `~/Documents/notes`). Home-relative → resolves
  on both OSes, no machine-specific path. denote/capture/agenda all derive from it.
- **Syncthing on nix hosts: HM `services.syncthing` in the shared `home.nix`** —
  cross-platform (launchd on macOS, systemd on NixOS; verified both supported in HM
  25.11). **Declarative** devices + `~/notes` folder, `overrideDevices`/
  `overrideFolders = true` (nix is source of truth; GUI drift reset on rebuild).
- **mnemosyne (Synology)** runs Syncthing via its Package Center / SynoCommunity
  (or Docker) + the official **Tailscale** package — configured in DSM, outside nix.
- **iPhone (theseus) is OUT of scope for 06** — deferred to a follow-on: a WebDAV
  server (or organice PWA) on mnemosyne, reached over Tailscale, with plain-org /
  organice as the client. 06's acceptance is mac↔NixOS only. Syncthing's iOS story
  is weak (Möbius, foreground-only), so the phone gets a different self-hosted path
  later; the architecture here (canonical node) already supports bolting it on.

## Slices

- **6a — org-directory → `~/notes`.** doom/config.el done; migrate existing
  `~/Documents/notes → ~/notes` on hestia (becomes the Syncthing seed).
- **6b — Tailscale on the missing nodes.** athena via nix (`services.tailscale.enable`
  + one `tailscale up`); mnemosyne via Synology's Tailscale package. Verify all four
  (hestia/athena/mnemosyne/theseus) on the tailnet.
- **6c — Syncthing declarative in `home.nix`.** Enable `services.syncthing` on both
  desktops; first-run to read each device ID; declare the 3 devices + `~/notes`
  folder (override = true).
- **6d — mnemosyne Syncthing.** Install on the Synology, grab its device ID (feed
  into 6c), accept the desktop devices + share `~/notes` as the canonical copy.
- **6e — verify.** Edit a note on hestia → appears on athena (relayed via mnemosyne
  with the other desktop off).

## Build progress — PAUSED 2026-06-11 (resume when home)

**Nix changes already made (in repo, on `feat/nixos-host`):**
- `doom/config.el`: `org-directory` → `~/notes` (6a). ✓
- `hosts/athena/default.nix`: `services.tailscale.enable = true;` (6b). ✓
- `home.nix`: `services.syncthing` enabled, `overrideDevices`/`overrideFolders =
  true`, but `settings.devices` + `settings.folders` are **empty placeholders** —
  to be filled with real device IDs (6c). Eval-checked; hestia builds. ✓

**Remaining (all need home access to mnemosyne/athena):**
1. **6a file move (hestia, can do anywhere):** `mkdir -p ~/notes && rsync -a
   ~/Documents/notes/ ~/notes/`; verify; remove old. Restart Emacs (config is
   out-of-store, no rebuild needed).
2. **6b Tailscale:** athena `rebuild` → `sudo tailscale up`; mnemosyne install
   Synology Tailscale pkg + sign in. Confirm `tailscale status` shows hestia/
   athena/mnemosyne/theseus.
3. **6c device IDs:** `rebuild` both desktops (starts syncthing) → collect each
   device ID (`http://127.0.0.1:8384` Actions→Show ID, or `syncthing --device-id`).
4. **6d mnemosyne Syncthing** — **OPEN DECISION:** Docker (Container Manager) vs
   SynoCommunity package. Recommended **Docker IF the model supports Container
   Manager** (x86 Intel Synology w/ enough RAM) — official image tracks upstream,
   keeps version parity with the nixpkgs-25.11 desktops; SynoCommunity often lags.
   Else **SynoCommunity** (one-click, safe default). Docker gotchas: set PUID/PGID
   to the DSM user owning the notes share; bind-mount the share; expose GUI 8384.
   Get mnemosyne's device ID, share `~/notes` as canonical.
5. **Then:** hand the 3 device IDs back → fill the declarative
   `services.syncthing.settings.{devices,folders}` block in `home.nix` → `rebuild`
   both → verify (6e).

**To resume:** check Synology model for Container Manager support (decides 6d), do
steps 1–4, return with the three device IDs.

## Acceptance criteria

- [ ] Org files live in one syncable location, available on both machines
- [ ] Editing a note on one machine appears on the other after sync
- [ ] Org config (load paths, agenda files) is placed by home-manager, cross-platform
- [ ] Paths resolve correctly on both macOS and NixOS (no hardcoded machine-specific paths)

## Blocked by

- #04 widen home-manager passthrough
- #05 basic NixOS config
