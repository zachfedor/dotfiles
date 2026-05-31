# Diagnose & fix Emacs startup (Chemacs2 + Doom)

Status: done

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

---

**Second fault (GUI only): private config not loading in `/Applications/Emacs.app`.**
Symptoms: switching to a project ran the default file prompt instead of the custom
`magit-status` action (`config.el:293`), and `config.el:1` showed a "Doom hasn't
been initialized; run `doom sync`" lint.

Root cause: macOS GUI apps launched from the Dock get launchd's environment, not the
shell's, so `DOOMDIR` (exported only in the shell) is invisible to Emacs.app. Doom's
`doom-user-dir` resolution (`lisp/doom.el:254`) is `DOOMDIR → ~/.config/doom →
~/.doom.d`. With no `DOOMDIR` and neither default dir present, it resolved to the
non-existent `~/.doom.d`, so the real config at `~/.dotfiles/doom` never loaded and
`+workspaces-switch-project-function` stayed at its default. The CLI `emacs` worked
the whole time because the shell exports `DOOMDIR`. (The deleted `~/.doom.d` symlink
was already dangling — its target `~/.dotfiles/doom.d` doesn't exist — so it was
irrelevant.) Both binaries are the same `emacs-plus@30` 30.2; no version mismatch.

Fix applied (env-independent): symlink Doom's default location to the real config:

```sh
ln -s ~/.dotfiles/doom ~/.config/doom
```

Verified: a daemon launched with `env -u DOOMDIR` (mimicking the Dock app) now
resolves `doom-user-dir=~/.config/doom`, `switch-fn=magit-status`, magit loaded.

**Reproducibility (for #04, home-manager):**
- Place `~/.config/doom → ~/.dotfiles/doom` via home-manager (the dead, Chemacs-era
  Doom block in `install.sh:326-341` should not be revived).
- Run `doom env` so GUI launches inherit shell `PATH` (git/fd/rg) for magit/LSP.
