# #10 audit — decision ledger (grilling session)

Verdict per tool: **declare** (which layer) / **ephemeral** (brew survives, ADR-0003) / **cut** (uninstall). Simplify-biased: cut is default.

## Policy (cross-cutting)
- One **Flake**; two host configs: `darwinConfigurations.hestia` (mac), `nixosConfigurations.athena` (NixOS).
- Layers: CLI/cross-platform → `home.nix` (both hosts); mac GUI/mac-only → `hosts/hestia/default.nix` casks/brews; services → nix-darwin system layer.
- App-internal state (extensions, bookmarks, vaults, logins) is OUT of nix scope → app's own cloud sync; capture "sign into X" in `docs/new-host.md`. Pull a config file into nix only when it's a real dotfile worth cross-machine parity.

## Tier 1 — essentials (DONE)
| Tool | Verdict | Layer / note |
|---|---|---|
| Firefox | declare | hestia cask (already) — daily driver |
| Safari | out of scope | system, not nix |
| Brave | declare | hestia cask (add) — OSS chromium for testing |
| Chrome, Edge, Opera, Opera-GX | cut | redundant browsers |
| 1Password GUI | declare | hestia cask |
| 1Password CLI (`op`) | declare | `home.nix` `_1password-cli` (used) |
| Slack | declare | hestia cask (already) |
| Discord | declare | hestia cask (already) |
| Telegram | declare | hestia cask (add) |
| Zoom | cut | install on-demand if needed |
| Dropbox | cut | archive `~/Library/CloudStorage/Dropbox` (6MB) → `~/archive/dropbox-import/` first |
| Nextcloud | cut | no local data; experiment |
| Obsidian, Logseq, Notion | cut | pure org/PARA is the system |
| Tailscale | declare | nix-darwin `services.tailscale.enable` on hestia; cut both casks (CLI-only, no GUI) |

## Tier 2 — dev tooling (in progress)
Architecture: per-project nix devShells (flake + `.envrc use flake`, nix-direnv) per ADR 04d. Global = thin fallback only.
| Tool | Verdict | Layer / note |
|---|---|---|
| pyenv-virtualenv, rbenv, n | cut | version managers → devShells |
| python@3.9, python@3.10, ruby@3.0 | cut | pinned runtimes → devShells |
| python3, nodejs | declare | `home.nix` — global scratch fallback |
| clojure, clojure-lsp, leiningen | cut from global | move to devShells (REMOVE current home.nix decl) |
| rust/cargo/rust-analyzer | devShell only | work uses rust; per-project flake, no global rustup |
| deno, pnpm, composer, openjdk | cut from global | per-project devShells |
| elisp | n/a | native to Emacs (ielm), zero deps |
| Doom `:tools direnv` | ENABLE (user doing) | uncomment init.el:95 + `doom sync` — required so envrc surfaces devShell toolchains to LSP |
| `doom env` baseline | keep | re-run after home.nix PATH changes (layer-1 global env for GUI Emacs) |

### Two-layer Emacs/devShell env model (ADR candidate)
- Layer 1 `doom env` → snapshots login-shell PATH into GUI Emacs (global baseline; re-run on PATH change).
- Layer 2 `:tools direnv`/envrc → applies each project's devShell env per-buffer → LSP finds per-project toolchains.

### Tier 2 — dev CLIs & toolchains
| Tool | Verdict | Layer / note |
|---|---|---|
| Docker Desktop (`Docker.app`) | cut | → colima |
| colima | declare | mac: hestia layer (Linux VM); athena: `virtualisation.docker.enable` |
| docker, docker-compose CLI | declare | `home.nix` (cross-host docker command) |
| VSCode, Zed | cut | Emacs primary, neovim backup |
| iTerm2 | cut | alacritty + tmux |
| Alacritty Quake mode | BACKLOG bonus | Hammerspoon toggle (mac); hyprland special workspace on athena (#08) |
| gh, jq, flyctl, wget | declare | `home.nix` global CLIs (`wget` not `wget2`) |
| awscli, mkcert, imagemagick, hugo, subversion, exercism, dasel | cut | unused / jq covers dasel |
| gnu-sed, gnu-tar, grep, awk | cut | coreutils/gnutar in nix; revisit if a script needs GNU flags |
| black, isort, golangci-lint, gopls, composer, cmake | cut from global | per-project devShells |
| avrdude, dfu-programmer, teensy_loader_cli | cut from global | → QMK devShell (`qmk` nixpkgs) when flashing |

## Tier 3 — workflow utilities (DONE)
| Tool | Verdict | Layer / note |
|---|---|---|
| rcm | cut | predecessor dotfile mgr; home-manager supersedes |
| reattach-to-user-namespace | cut | unneeded on modern macOS tmux |
| libass, nghttp2, hwloc, libvterm, gobject-introspection, nss, libfido2, texinfo, ghostscript | cut | orphaned deps → `brew autoremove` |
| aspell | declare | `home.nix` — Emacs flyspell (+dicts) |
| blueutil | declare | `home.nix` darwin-scoped — Hammerspoon scripting |
| caffeine (cask) | cut | native `caffeinate` CLI; bind via Hammerspoon |
| wordnet | cut | niche |
| übersicht (cask) + `ubersicht/` repo dir + widgets | cut | dead widgets; remove repo dir too |
| unified-remote (cask) | cut | dormant |
| hammerspoon, pandoc, tree, htop, gifsicle, html-tidy | keep | already declared |
## Tier 4 — entertainment (in progress)
Policy: declare cask (reproduce on fresh box) / ephemeral (brew survives, not reproduced) / cut. Vendor-installer apps = out of nix scope, manual, listed in `docs/new-host.md`.
| App | Verdict | Note |
|---|---|---|
| Steam, OpenEmu, OpenMW | declare casks | games/emulation |
| CrossOver + Age of Empires 2 + any Wine tooling | cut | paid Wine wrapper; remove `~/Applications/CrossOver*` too |
| VCV Rack 2 Free | declare cask | active modular-synth use |
| Anki | declare → `home.nix` | nixpkgs, cross-platform, AnkiWeb sync |
| OBS | declare cask | screen recording |
| Transmission | declare cask | torrents |
| SABnzbd | declare cask | usenet |
| HandBrake | cut | |
| Kobo | cut | calibre covers ebooks |
| Amazon Kindle | vendored note | MAS, out of scope |

### Vendored apps (out of nix scope — manual reinstall, list in docs/new-host.md)
- Music: Ableton Live 12 Lite, GarageBand, Melodics, Akai Professional, inMusic Software Center, Studio Instrument Collection
## Tier 2 stragglers (caught mid-sweep)
| Tool | Verdict | Layer / note |
|---|---|---|
| mysql, mysql@8.4, postgresql@14, redis | cut | per-project DBs via colima/docker-compose; no always-on local DB |
| tmux | declare | `home.nix` (config already passthrough; binary was brew-only) |
| make | cut | Xcode CLT / devShell provides |
| ngrok | declare | `home.nix` (nixpkgs CLI, unfree) |
| editorconfig (brew) | cut | dup of nix editorconfig-core-c |
| ncdu | declare | `home.nix` — replaces JDiskReport |

## Tier 5 — non-essentials / cruft sweep
| App | Verdict | Note |
|---|---|---|
| GIMP, Inkscape | declare casks | creative work |
| JDiskReport | cut | → ncdu |
| Microsoft AutoUpdate | cut | orphan (no Edge/Office) |
| Mozilla VPN | cut | Tailscale covers needs |
| Tunnelblick | cut | Tailscale covers needs |
| Pages, Numbers, Keynote, iMovie | cut | iWork/iLife unused |
| Amazon Kindle | cut | |
| GarageBand | keep (vendored) | uses it alongside Ableton |

## Homebrew activation flags (#10 known item)
- `onActivation.upgrade = false`, `autoUpdate = false` — rebuilds fast/predictable; `brew upgrade` manually when wanted.

## Emacs migration (#10 headline)
- emacs-plus@30 + `d12frosted/emacs-plus` tap → **cut**. Move to nixpkgs `emacs30` (Cocoa/NS) in `home.nix` darwin branch; athena keeps `emacs30-pgtk`. Then `doom sync` + `doom env`, re-verify Doom. ADR candidate.

## Vendored apps (out of nix scope — manual reinstall, list in docs/new-host.md)
- Music: Ableton Live 12 Lite, GarageBand, Melodics, Akai Professional, inMusic Software Center, Studio Instrument Collection
- Hardware: Logi Options, LogiTune (Logitech)
- Printing: PaperCut Hive
- Apple iWork/iLife (MAS, free): Keynote, Numbers, Pages, iMovie
- Reading: Amazon Kindle (MAS)
- Claude Code URL Handler (managed by Claude Code itself)
## Tier 5 — non-essentials (pending)
