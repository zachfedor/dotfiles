# Refine zsh/zim config

Status: done

## What to build

Polish the newly migrated zsh + zim setup (zshrc, zshenv, zprofile, zimrc).
Mostly works already; this is content-only cleanup — no placement changes, so it
is not invalidated by the later home-manager migration (per ADR-0001).

## Acceptance criteria

- [x] zsh starts with no errors or warnings on a fresh shell (~0.19s)
- [x] Load order across zshenv / zprofile / zshrc is correct and documented
      (already documented in each file's header; verified zshenv→zprofile→zshrc)
- [x] Dead config left over from the bash/oh-my-zsh era is removed
- [x] Aliases, prompt, and vi-mode binds behave as intended

## Blocked by

None - can start immediately

## Comments

Shell was already healthy (no startup errors, ~0.19s). Changes were
cross-platform hardening + cleanup:

- **A — guarded `stellar` completion** (`zshrc`): `command -v stellar >/dev/null &&
  source <(stellar completion --shell zsh)`. Was unguarded → would error on any
  machine without `stellar` (NixOS/fresh). The one real cross-OS bug.
- **D — consolidated vi-mode history search** (`zshrc`): replaced the literal
  `incremental-search` binds + the dead commented block with the **pattern**
  variants (glob-aware), added `unsetopt FLOW_CONTROL` so **Ctrl-S forward search
  actually works** (terminal XOFF was eating it), and kept vim-normal-mode `/`
  (forward) and `?` (backward) history search. Root cause of the old "Ctrl-R does
  nothing": `bindkey -v` drops zsh's default Ctrl-R; vi insert mode needs the
  explicit rebind.
- **B — `aliases` shebang** `#!/bin/bash` → `#!/usr/bin/env zsh` (file is sourced
  by zsh, not executed by bash).
- **C — `DOOM_BIN`** repointed `~/code/doomemacs/bin` → `~/.emacs.d/bin` (the
  canonical standard path after issue 01's relocation).

Verified at runtime: all binds active in the right keymaps, `FLOW_CONTROL` off,
stellar guard skips cleanly when absent, `~/.emacs.d/bin` resolves.

**Flagged for #08 (Flavours/ricing):** `base16-shell` block (`zshrc:~90`) is legacy
theming, overlaps the ricing tool. Left as-is (already `-s`-guarded, no error);
fold into the ricing decision.

**Prompt tweak:** the zim `minimal` theme put the working directory in the RIGHT
prompt (`RPS1`), easy to lose on wide screens. Moved it to the LEFT, kept dim
(grey 244) and trimmed to the last 2 dirs; git status stays on the right.
Implemented as a post-`zim init` override in `zshrc` (PS1/RPS1 reassignment) so
zim module updates don't clobber it — the theme file itself is untouched.

**Considered, declined:** dev add-ons (AUTO_CD/AUTO_PUSHD setopts, zoxide, fzf,
direnv). User wanted none as standalone shell additions. `direnv` noted on #03 to
add alongside the Nix flake instead.
