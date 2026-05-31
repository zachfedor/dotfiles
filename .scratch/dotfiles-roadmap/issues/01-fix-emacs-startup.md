# Diagnose & fix Emacs startup (Chemacs2 + Doom)

Status: resolved

## What to build

Restore a clean Emacs launch. Chemacs2 was newly installed and Doom was just
upgraded; both fail or misbehave on startup. Likely shared root cause — Chemacs2
profile selection breaking the Doom profile load. Diagnose the two together,
treat as one investigation, land the fix that gets the default profile booting.

## Acceptance criteria

- [x] `emacs` launches the intended default profile with no startup errors
- [x] Doom (post-upgrade) loads cleanly; `doom doctor` passes (or remaining
      warnings are understood and documented)
- [x] Chemacs2 `~/.emacs-profiles.el` selects the correct default profile
- [x] Root cause noted in the issue comments (for future profile work, #09)

## Blocked by

None - can start immediately

## Comments

**Root cause (not the Chemacs/profile theory):** Doom was originally installed at
`~/.config/emacs`; straight-build baked **absolute** symlinks into that path. Doom
later moved to `~/code/doomemacs` (the Chemacs `default` profile) and
`~/.config/emacs` was deleted. That left **652 of 1122** straight build symlinks
dangling into the dead tree (incl. `general`), crashing startup with
`File is missing: .../general/general.el.gz`. The straight *repos* were intact —
only the *build* layer was stale, and `doom sync` alone didn't repair it because
straight's build-cache believed the builds existed.

**Fix applied:**

```sh
rm -rf ~/code/doomemacs/.local/straight/build-30.2 \
       ~/code/doomemacs/.local/straight/build-30.2-cache.el
~/code/doomemacs/bin/doom sync
```

Verified: dangling symlinks 652 → 0; `general.el` build target valid; `doom
doctor` shows no runtime errors (only benign Chemacs warnings); daemon boot loaded
212 packages across 52 modules with no errors.

**Also cleaned up:** removed legacy `~/.doom.d → .dotfiles/doom.d` symlink (private
config is `DOOMDIR=~/.dotfiles/doom`; the old symlink was ambiguous and ignored).

**Non-blocking follow-ups noted:** `setq!` obsolete-alias warning in Doom's clojure
module (upstream, cosmetic); projectile config points at non-existent `~/git`.
