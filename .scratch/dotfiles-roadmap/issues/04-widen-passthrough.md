# Widen home-manager passthrough to all macOS configs

Status: in-progress

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
- [ ] **4d — CLI tools → `home.packages`** (curated; I propose groups, you cut).
- [ ] **4e — GUI casks + fonts → nix-darwin `homebrew` module** (curated). Drop
      ubersicht. Delete install.sh at the end. **Must include a Nerd Font** (e.g.
      fira-code or hack nerd font) — nvim glyphs depend on it (deferred from 4b).

## Notes (carry-ins)

- From #01: `~/.config/doom → ~/.dotfiles/doom` (done, out-of-store) + run
  `doom env` so GUI inherits shell PATH. **`doom env` still TODO** — do in 4d once
  the CLI toolchain (git/fd/rg) is nix-managed.
- From #03: add **direnv** + nix-direnv (4d) for `use flake` per-project envs.
