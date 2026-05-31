# Widen home-manager passthrough to all macOS configs

Status: needs-triage

## What to build

With the pipeline proven (#03), bring the remaining configs under home-manager
passthrough — vim, tmux, alacritty, git, emacs profiles, hammerspoon, scripts —
and fully retire `install.sh`. Configs stay verbatim; only placement moves. No
native-module translation here (per ADR-0001, that is opt-in later).

## Acceptance criteria

- [ ] All currently symlinked configs are placed by home-manager
- [ ] `install.sh` is removed (or reduced to a one-line bootstrap that installs Nix)
- [ ] A fresh `home-manager switch` reproduces the full macOS user config
- [ ] No config content changed in the move (passthrough verified)

## Blocked by

- #03 home-manager flake tracer
