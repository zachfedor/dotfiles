# Refine zsh/zim config

Status: needs-triage

## What to build

Polish the newly migrated zsh + zim setup (zshrc, zshenv, zprofile, zimrc).
Mostly works already; this is content-only cleanup — no placement changes, so it
is not invalidated by the later home-manager migration (per ADR-0001).

## Acceptance criteria

- [ ] zsh starts with no errors or warnings on a fresh shell
- [ ] Load order across zshenv / zprofile / zshrc is correct and documented
- [ ] Dead config left over from the bash/oh-my-zsh era is removed
- [ ] Aliases, prompt, and vi-mode binds behave as intended

## Blocked by

None - can start immediately
