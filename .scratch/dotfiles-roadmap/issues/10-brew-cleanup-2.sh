#!/usr/bin/env bash
# issue 10 brew cleanup PASS 2 — run after pass 1 + rebuild. Removes:
#   (a) brew↔nix duplicates (nix provides; brew copy shadowed on PATH)
#   (b) orphan leaves exposed when pass-1 removals dropped their dependents
#   (c) the tailscale cask (nix-darwin runs the service now)
# NOT touched: font-* casks (some lack a nix equivalent — separate font task),
# and zork (no nixpkgs). No sudo needed. `|| true` so one miss doesn't abort.
set -u

echo "== tailscale cask (nix service replaces it) — quit the app first if it fails =="
osascript -e 'quit app "Tailscale"' 2>/dev/null || true
brew uninstall --cask tailscale 2>/dev/null && echo "  removed cask tailscale" || echo "  (tailscale cask not removed — quit the menubar app, rerun)"

echo "== alacritty cask (nix provides the binary) =="
brew uninstall --cask alacritty 2>/dev/null && echo "  removed cask alacritty" || true

echo "== brew↔nix duplicate formulae =="
for f in coreutils htop p7zip tree-sitter ; do
  brew uninstall "$f" 2>/dev/null && echo "  removed dup $f (nix provides)" || true
done

echo "== orphan / leftover formulae (zero reverse-deps) =="
for f in boost cffi gcc go php pyenv tesseract gnupg \
         gobject-introspection hwloc libass libfido2 libheif librsvg \
         libvterm nghttp2 nss ; do
  brew uninstall "$f" 2>/dev/null && echo "  removed $f" || true
done

echo "== final autoremove + report =="
brew autoremove 2>&1 | tail -10
echo
echo "Expected remaining leaves: just 'zork'. Casks: the declared set + font-*."
echo "brew leaves:"; brew leaves 2>/dev/null | tr '\n' ' '; echo
echo "Then 'brew cleanup' to reclaim disk."
