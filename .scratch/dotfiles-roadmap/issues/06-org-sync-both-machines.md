# Org notes: sync + open on both machines (tracer)

Status: needs-triage

## What to build

The demoable tracer for the Org-mode goal: choose and stand up the sync mechanism
(e.g. git, Syncthing, or a cloud dir) for org files, and have home-manager place
the org config on both machines so the same notes open on macOS and NixOS.
Portability is baked in from the foundation, not retrofitted.

## Acceptance criteria

- [ ] Org files live in one syncable location, available on both machines
- [ ] Editing a note on one machine appears on the other after sync
- [ ] Org config (load paths, agenda files) is placed by home-manager, cross-platform
- [ ] Paths resolve correctly on both macOS and NixOS (no hardcoded machine-specific paths)

## Blocked by

- #04 widen home-manager passthrough
- #05 basic NixOS config
