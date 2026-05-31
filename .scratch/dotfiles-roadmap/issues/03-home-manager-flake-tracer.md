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

## Notes

- **direnv**: consider adding `direnv` + nix-direnv during the Nix work so a
  `.envrc` with `use flake` auto-loads a project's toolchain on `cd`. Strong
  synergy with the flake; hook into zsh (`eval "$(direnv hook zsh)"`) and place
  both via home-manager (carry into #04). Deferred from the issue-02 zsh pass —
  not wanted as a standalone shell addition, only alongside Nix.
