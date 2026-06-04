# Widen home-manager passthrough to all macOS configs

Status: done (pending final 4e activation)

## What to build

With the pipeline proven (#03), bring the remaining configs under home-manager
passthrough — vim, tmux, alacritty, git, emacs profiles, hammerspoon, scripts —
and fully retire `install.sh`. Configs stay verbatim; only placement moves. No
native-module translation here (per ADR-0001, that is opt-in later).

## Acceptance criteria

- [x] All currently symlinked configs are placed by home-manager (4a)
- [x] `install.sh` removed — deleted entirely; bootstrap documented in README (4e)
- [x] A fresh rebuild reproduces the full macOS user config (verified each slice)
- [x] No config content changed in the passthrough move (4a)

## Blocked by

- #03 home-manager flake tracer

## Slices (grilled — full retire + extras)

Scope expanded beyond original: full install.sh retire, a minimal neovim backup
config, drop ubersicht, and handle ssh + packages the nix/nix-darwin way. Done as
ordered, separately-verifiable rebuilds. Placement = **store passthrough for all**
(the nix way; edits apply on `darwin-rebuild`), EXCEPT `~/.config/doom` which stays
**out-of-store** (`mkOutOfStoreSymlink`) so the Emacs GUI/CLI never diverge between
rebuilds (issue 01 guarantee).

- [x] **4a — widen dotfile passthrough + retire symlink loop.** All configs moved
      to `home.nix`: zsh×5, git×2, tmux, vim×3, alacritty, hammerspoon (store);
      doom (out-of-store). `~/.vim` real dir → backed up `.vim.hm-bak`. Pre-existing
      install.sh symlinks for `alacritty`/`hammerspoon` (symlink-to-dir) had to be
      `rm`'d first — HM `backupFileExtension` can't back those up. install.sh
      symlink loop + vim-plug + `~/.vim/tmp` neutralized. Activated, verified.
- [x] **4b — minimal neovim backup config.** Native `programs.neovim` in home.nix,
      mirrors Doom's TOOLS (evil native, SPC leader, telescope≈vertico,
      nvim-tree≈treemacs, cmp≈corfu, which-key, treesitter, gitsigns, todo-comments,
      lualine). Theme: built-in `habamax` via `pcall` for now (real theme → #08).
      **vim DROPPED entirely** (not just fixed) — nvim's viAlias/vimAlias make
      `vi`/`vim` launch nvim; removed vimrc/vimrc_background/vim/ from repo +
      home.nix, `brew uninstall vim`. Gotcha hit: `nvim` resolved to the shadowing
      **brew** neovim (no plugins → `nvim-treesitter.configs not found`); fixed by
      `brew uninstall neovim` — see [[brew-to-nix-migration-rule]]. Activated, tests
      pass. **Deferred to 4e:** a Nerd Font (nvim-tree/lualine glyphs render broken
      without one).
- [x] **4c — ssh via `programs.ssh`.** Config declarative in home.nix; keys stay
      manual (out of flake). Verified `ssh -T git@github.com` authenticates. Removed
      repo `ssh/` dir + the install.sh ssh block (keygen kept as a manual comment).
      Gotcha: HM 25.05 has no `enableDefaultConfig`/`matchBlocks` collision toggle —
      a `matchBlocks."*"` emits a *second* `Host *` stanza that conflicts with HM's
      built-in defaults. Fix: set the **top-level** `programs.ssh` options (they ARE
      the default `Host *` block) + `extraConfig` for IdentityFile/UseKeychain.
      `UseKeychain` is macOS-only — guard for NixOS in #05.
- [x] **4d — CLI tools → `home.packages`** (curated). Kept: dev core (git,
      coreutils, gnutar, fd, ripgrep, silver-searcher, tree, tree-sitter,
      editorconfig, htop, openssl, pandoc, p7zip), shellcheck, shfmt, clojure +
      clojure-lsp + leiningen (guile dropped), lua, ffmpeg, gifsicle, html-tidy,
      nethack. Dropped: JS globals (node→nix-direnv per-project), n/avn, astrum,
      heroku, sqlite/postgres/redis. **mise dropped** → `programs.direnv` +
      nix-direnv (per-project `use flake`; chosen over mise for pinned/reproducible
      cross-machine envs). Removed mise from zimrc + brew. brew-uninstalled all
      migrated tools incl. git/clojure-lsp-native (see migration rule). Ran
      `doom env` (refresh PATH snapshot). direnv hook added manually to zshrc
      (passthrough zsh ≠ HM-managed, so no auto-hook — see ADR-0001 coupling note).
      Also fixed nix-darwin `/etc/zshrc` double-`compinit` (zim warning) via
      `programs.zsh.enableCompletion = false`. zork has no nixpkgs pkg → stays brew
      (fold into 4e homebrew module).
- [x] **4e — GUI casks + fonts + install.sh retired.** Split by the right axis:
      - **Fonts + alacritty binary → `home.packages`** (nix, cross-platform): Nerd
        Fonts (fira-code, hack) for editor/terminal glyphs + web-dev typefaces
        (fira-*, source-*, inconsolata, roboto, lato, lora, merriweather, vt323).
        `et-book` dropped (not in nixpkgs 25.05).
      - **GUI casks → nix-darwin `homebrew` module** (macOS-only by nature; NixOS
        gets these from nixpkgs in #05): firefox, slack, discord, vlc, calibre,
        steam, openemu, hammerspoon. Brews: emacs-plus@30 (tap d12frosted/emacs-plus,
        from-source — Doom), zork (no nixpkgs pkg). One-tool-per-job; dropped
        chrome/iterm2/obs/dropbox/transmission/kobo/jdiskreport/virtualbox.
      - **`cleanup = "none"`** (KEY decision): rebuilds never auto-uninstall brew,
        so manual `brew install --cask` tests survive — preserving a nix-like
        declared=permanent / ad-hoc=ephemeral split for GUI apps. CLI testing uses
        `nix shell`/`nix run` instead. Manual `brew bundle cleanup` (dry-run) to
        prune drift on demand. Documented in README.
      - **install.sh retired**: deleted install.sh + install-utils.sh +
        install.sh.backup. ubersicht/ kept for historical reference (config only).
      - README rewritten for the nix-darwin/home-manager reality + habits.

## Notes (carry-ins)

- From #01: `~/.config/doom → ~/.dotfiles/doom` (done, out-of-store) + run
  `doom env` so GUI inherits shell PATH. **`doom env` still TODO** — do in 4d once
  the CLI toolchain (git/fd/rg) is nix-managed.
- From #03: add **direnv** + nix-direnv (4d) for `use flake` per-project envs.
