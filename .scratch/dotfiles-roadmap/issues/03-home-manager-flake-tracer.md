# home-manager flake: one config end-to-end on macOS (tracer)

Status: needs-triage

## What to build

The tracer bullet for ADR-0001. Stand up a minimal Nix flake with home-manager
on macOS that places exactly one existing config (e.g. `zshrc`) via passthrough
(`home.file."...".source = ./zshrc`). Prove the whole pipeline end-to-end before
widening. `install.sh` is retired only for that one path.

## Acceptance criteria

- [ ] `flake.nix` + minimal `home.nix` build with `home-manager switch`
- [ ] One config (zshrc) is placed by home-manager, content unchanged (passthrough)
- [ ] The file edits in this repo and re-applies cleanly on rebuild
- [ ] `install.sh` no longer manages that one config; everything else untouched
- [ ] README/CONTEXT note how to run `home-manager switch`

## Blocked by

None - can start immediately
