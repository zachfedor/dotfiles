# Base16 theme switching / script-based ricing tool

Status: done

## What to build

Set up Flavours (or an equivalent script-based theming tool) to drive consistent
colorschemes across terminal, editor, and other configs. Not urgent.

## Acceptance criteria

- [x] Flavours (or chosen tool) installed via the flake / home-manager
- [x] At least one scheme applies across alacritty + vim (+ others as relevant)
- [x] Theme switching is a single command and is reproducible

## Blocked by

None - can start immediately

## Build notes (2026-07-22 → 2026-07-26)

First pass built on Flavours (a Rust base16 CLI) per the original
docs/adr/0009. That fell apart on rebuild: Flavours' docs describe a scheme
schema its 0.7.1 binary doesn't actually accept, and its config directory is
hardcoded per-OS with no override flag on macOS — both surfaced as real
`flavours apply` failures, not just theory. Digging further turned up
Flavours' documented Nix companion repo archived, open Emacs/stability issues
unanswered, and the maintainer not dogfooding it in their own dotfiles.
Replaced it entirely (docs/adr/0009-base16-theme-switching.md, rewritten) with
a self-authored `scripts/theme-switch` (Python stdlib, no external theming
binary) that reads flat base16 yaml directly and writes the three adapter
files itself.

Requirements were also revisited with the user beyond the original issue
text: open-ended scheme sourcing (no curated allowlist), an OS-driven
light/dark **variant** switch layered on top of a manually-chosen **family**
(e.g. Nord light ↔ dark follows the system, Nord → Gruvbox stays manual — the
OS watcher itself is deferred, see the ADR), and wallpaper-seeded palettes
(also deferred). `theme/schemes/*.yaml` ships onedark (default)/gruvbox/nord/
solarized-dark+light as a starting set, not a fixed list — dropping in
another base16 yaml is enough.

Also fixed along the way: `vimrc_background` was dead code left behind by
issue #04b's drop of classic Vim for native Neovim — deleted it, the Neovim
adapter wires into `programs.neovim`'s `initLua` instead. Doom's `doom-theme`
switched from `doom-one` to a generic `base16-theme`-backed theme
(`doom/themes/base16-dotfiles-theme.el`); `doom-color` calls in config.el
became `zf/base16-color`. `scripts/theme-switch <family>` /
`--variant dark|light` is the single-command entrypoint (repo `scripts/` is
already on `PATH`).

**Needs a rebuild to activate** (`rebuild` alias / `darwin-rebuild switch
--flake ~/.dotfiles#hestia`): the new `home.activation.themeDefault` hook runs
`scripts/theme-switch onedark` so the generated adapters exist before Emacs/
Alacritty/Neovim are next started.

**Rebuild-and-verify round on hestia (2026-07-26 → 2026-08-09), user-driven:**
- Rebuild initially failed: `~/.config/alacritty` was still a *directory-level*
  passthrough symlink from before this issue (into the read-only nix store),
  so home-manager couldn't move/backup/symlink the new *file-level*
  `alacritty.toml` inside it — same class of gotcha issue #04a already
  documented for alacritty/hammerspoon. One-time manual fix:
  `rm ~/.config/alacritty` (removes the stale HM-created symlink, not user
  data) before rebuilding.
- `theme-switch` itself worked once rebuilt. Emacs hit
  `(void-variable base16-dotfiles-theme-colors)`: `config.el`'s
  `custom-set-faces!` block calls `zf/base16-color` *immediately* at
  config.el-eval time, but Doom doesn't `load-theme` (which is what binds
  that variable, via `doom/themes/base16-dotfiles-theme.el`) until
  `window-setup-hook` — well later. Fixed by splitting the color-loading
  logic into `doom/themes/base16-dotfiles-colors.el`, loaded eagerly by both
  `config.el` (immediate binding) and `base16-dotfiles-theme.el` (so
  `zf/reload-base16-theme` / `SPC t r` still re-reads fresh colors off disk
  on every reload).
- Alacritty windows kept showing a different scheme than what `colors.toml`
  had — traced to a **pre-existing, unrelated legacy mechanism**:
  `zshrc` had a `base16-shell` block (chriskempson/base16-shell, manually
  cloned to `~/.config/base16-shell` — noted as legacy debt back in issue #02's
  build notes and explicitly flagged there for #08) that fires on every
  interactive shell startup and repaints the terminal's 16 ANSI colors via raw
  OSC escape codes from a stale cached `~/.base16_theme`, independent of and
  overriding Alacritty's own config. Removed the block from `zshrc` (dead now
  that `theme-switch` owns terminal theming) and the stale reference in
  `docs/new-host.md`'s deferred-items list. The `~/.config/base16-shell` clone
  itself and `~/.base16_theme` symlink are inert leftovers, left alone (out of
  band, not repo-managed) — harmless once nothing sources them.
