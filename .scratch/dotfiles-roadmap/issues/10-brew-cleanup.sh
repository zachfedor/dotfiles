#!/usr/bin/env bash
# issue 10 brew cleanup — RUN AFTER a successful `rebuild` (darwin-rebuild switch),
# so the nixpkgs replacements + new casks are already installed before the brew
# copies are removed (no tool gap on PATH). brew uninstall needs no sudo.
#
# Review before running. `|| true` so one already-removed item doesn't abort.
set -u

echo "== casks (cut + migrated-to-nix) =="
for c in \
  google-chrome microsoft-edge opera opera-gx \
  dropbox nextcloud \
  obsidian logseq notion \
  iterm2 visual-studio-code \
  caffeine ubersicht unified-remote \
  crossover handbrake-app kobo \
  jdiskreport microsoft-auto-update tunnelblick \
  ngrok tailscale tailscale-app ; do
  brew uninstall --cask "$c" 2>/dev/null && echo "  removed cask $c" || true
done

echo "== formulae (cut) =="
for f in \
  pyenv-virtualenv rbenv n python@3.9 python@3.10 ruby@3.0 openjdk deno pnpm composer \
  mysql mysql@8.4 postgresql@14 redis \
  black isort golangci-lint gopls cmake make dasel hugo subversion exercism awscli mkcert imagemagick \
  avrdude dfu-programmer teensy_loader_cli \
  gnu-sed gnu-tar grep awk \
  rcm reattach-to-user-namespace wordnet wget2 editorconfig ghostscript texinfo ; do
  brew uninstall "$f" 2>/dev/null && echo "  removed formula $f" || true
done

echo "== formulae migrated to nixpkgs (#10) — drop brew copy to avoid PATH shadow =="
for f in gh jq tmux aspell blueutil flyctl docker-compose ; do
  brew uninstall "$f" 2>/dev/null && echo "  removed brew copy $f (nix provides)" || true
done

echo "== emacs-plus (ADR-0005: replaced by nixpkgs emacs30) =="
brew uninstall emacs-plus@30 2>/dev/null && echo "  removed emacs-plus@30" || true

echo "== sweep orphaned dependency libs =="
brew autoremove 2>&1 | tail -20

echo
echo "DONE. Verify: 'brew leaves' and 'brew list --cask' should now match the"
echo "declared set (zork + the hestia casks). Then: 'brew cleanup' to reclaim space."
