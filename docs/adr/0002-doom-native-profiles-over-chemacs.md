# Doom native profiles instead of Chemacs2

## Context

Chemacs2 was added recently (commit `0274d0d`) to allow switching between
multiple Emacs configs (Doom plus experiments). To do so it placed the Doom
framework at a **non-standard** location (`~/code/doomemacs`) and selected it via
`~/.emacs-profiles.el`.

That non-standard path was the root cause of two failures:

1. **GUI app loaded no private config.** `/Applications/Emacs.app` launched from
   the Dock inherits launchd's environment, not the shell's, so `DOOMDIR` was
   invisible and Doom couldn't find `~/.dotfiles/doom`.
2. **Persistent flycheck lint** ("Failed to load Doom… run `doom sync`"). Doom
   2.1.0's `+emacs-lisp--flycheck-non-package-mode` spawns `emacs -Q --batch`,
   which can't be passed `--init-directory` and always defaults
   `user-emacs-directory` to `~/.emacs.d` — where Doom no longer lived.

## Decision

Drop Chemacs2. Put the Doom framework at the **standard** `~/.emacs.d`
(symlinked to the existing `~/code/doomemacs` clone). Use **Doom's own native
profile system** (`$DOOMDIR/profiles.el`, selected via `emacs --profile NAME` /
`DOOMPROFILE`) for any future experimentation, including non-Doom configs.

The default "global" profile (plain `emacs`) is the stable daily driver; extra
profiles are opt-in and sandbox their packages under `$XDG_DATA_HOME/doom/NAME/`.

## Why

- Native profiles deliver the *only* Chemacs capability actually wanted —
  on-demand switching between configs (Doom or not) — **without** moving the
  framework off the standard path.
- Keeping the framework at `~/.emacs.d` is what fixes both bugs at the source:
  the GUI and the flycheck `-Q` subprocess both resolve Doom with zero env vars
  or flags. Verified: clean GUI-like boot (213 packages) and flycheck subprocess
  both pass with no env/flags.
- Stability of the daily driver was the priority; the experimentation Chemacs
  was bought for is the least-urgent goal (issue 09).

## Considered and rejected

- **Keep Chemacs, patch around it** (env injection for the GUI, `--init-directory`
  shims for flycheck) — treats symptoms; each new tool that spawns `emacs -Q`
  reintroduces the class of bug.
- **Move framework to `~/.config/emacs`** — still a standard-ish path, but the
  flycheck `-Q` subprocess defaults to `~/.emacs.d` regardless, so the lint would
  persist. `~/.emacs.d` specifically is required.

## Consequences

- `~/.config/doom → ~/.dotfiles/doom` (the private config) is retained; `DOOMDIR`
  in the shell becomes optional rather than load-bearing.
- The old `emacs-profiles.el` is removed from the repo and from `install.sh`;
  replaced by `doom/profiles.el`.
- `--init-directory` remains available for a one-off throwaway config without any
  profile machinery.
