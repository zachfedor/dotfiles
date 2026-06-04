# .dotfiles

Personal, declarative, cross-platform configuration built on **Nix** —
[home-manager](https://github.com/nix-community/home-manager) for user config and
[nix-darwin](https://github.com/nix-darwin/nix-darwin) for macOS system settings.

See `CONTEXT.md` for goals/vocabulary and `docs/adr/` for the decisions behind
this setup (most importantly ADR-0001: passthrough-first migration).

## Layout

- `flake.nix` — entry point; defines `darwinConfigurations.<host>` (currently
  `hestia`). Pins nixpkgs / nix-darwin / home-manager.
- `home.nix` — user config: dotfile placement, `home.packages` (CLI tools +
  fonts), `programs.*` (neovim, ssh, direnv).
- Plain config files (`zshrc`, `tmux.conf`, `alacritty/`, `doom/`, …) are placed
  **as-is** by home-manager (passthrough). Edit them directly as normal.
- `install.sh` — legacy bootstrap, being retired. Not used for config anymore.

## New machine (macOS)

1. Install Nix (Determinate): `curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install`
2. Clone this repo to `~/.dotfiles`.
3. Bootstrap nix-darwin (needs a real terminal for the sudo prompt):
   ```
   sudo nix run nix-darwin -- switch --flake ~/.dotfiles#hestia
   ```
4. Generate an SSH key (not managed by Nix — secrets stay out of the flake):
   ```
   ssh-keygen -t ed25519 -C "zachfedor@gmail.com"
   ssh-add --apple-use-keychain ~/.ssh/id_ed25519
   ```

## Daily use

**Apply config changes** (after editing `home.nix`, `flake.nix`, or a
store-passthrough config like `tmux.conf`):
```
sudo darwin-rebuild switch --flake ~/.dotfiles#hestia
```
Quote the flake ref if your shell globs `#`: `"$HOME/.dotfiles#hestia"`.

Exceptions that apply **without** a rebuild (out-of-store symlinks): `~/.config/doom`.

## Habits (how this stays clean)

**Testing a CLI tool — use Nix, not a permanent install:**
```
nix shell nixpkgs#ripgrep      # rg available in a subshell; exit = gone
nix run   nixpkgs#cowsay -- hi  # run once, nothing persists
```
To keep a CLI tool, add it to `home.packages` in `home.nix` and rebuild.

**Testing a GUI app** — Nix can't run `.app`s well on macOS, so use Homebrew
ad-hoc: `brew install --cask <app>`. With `cleanup = "none"` (see `flake.nix`),
manual installs **survive rebuilds** and are yours to remove. To make an app
permanent, add it to `homebrew.casks` in `flake.nix`.

**Brew reconciliation is manual on purpose.** Rebuilds never auto-uninstall brew
packages. To see/prune drift between the declared casks and what's installed, feed
the flake's generated Brewfile to `brew bundle cleanup` (dry-run by default):
```
# preview what isn't declared (safe — lists only):
nix eval --raw ~/.dotfiles#darwinConfigurations.hestia.config.homebrew.brewfile \
  | brew bundle cleanup --file=/dev/stdin
# actually prune: append --force
```

**Migrating a tool from brew → nix:** add it to nix → rebuild → verify the nix
copy wins on `PATH` → `brew uninstall <tool>` (else the brew binary shadows the
nix one). See ADR-0001.

## Platforms

- **macOS** (`hestia`) — primary. home-manager + nix-darwin.
- **NixOS** — planned second host; GUI apps come from nixpkgs there, defined
  separately (the macOS `homebrew` casks don't carry over).
