# Audit hestia installed tools/programs → nix config

Status: needs-triage

## What to build

Reconcile what's actually installed on hestia (the macOS / nix-darwin host)
against what's declared in nix. Several tools were installed out-of-band and
never added to config (Tailscale, 1Password are known examples; there are
likely more). Inventory the real machine state, diff it against the declared
config, and bring the gaps under declarative management in the correct layer:

- CLI tools / fonts → `home.nix`
- GUI apps → `hosts/hestia/default.nix` homebrew casks
- System daemons / services (e.g. Tailscale, 1Password) → nix-darwin system layer

End-to-end: a fresh `darwin-rebuild` reproduces every tool the user relies on,
with nothing important left to manual install.

## Known items to fix (surfaced during #06)

- **emacs-plus@30 / d12frosted tap — TEMP-disabled, must resolve.** During #06,
  `darwin-rebuild` started aborting on the homebrew step: Homebrew now refuses
  untrusted third-party taps, and `brew bundle` runs under sudo (root env) so a
  user-level `brew trust d12frosted/emacs-plus` doesn't apply. Worked around by
  commenting out `emacs-plus@30` + its tap in `hosts/hestia/default.nix` (it's
  already installed; `cleanup="none"` won't remove it). **Proper fix:** move Emacs
  to the nixpkgs `emacs` package (cross-platform, matches athena's emacs30-pgtk,
  drops the brew tap entirely) — then re-enable Doom. Until then hestia's Emacs is
  an undeclared brew install.
- **Tailscale (hestia) is a Homebrew install, not nix.** Confirmed during #06
  (`/opt/homebrew/bin/tailscale`, was "stopped"). Bring under nix-darwin.
- Revisit `homebrew.onActivation.upgrade = true` — it upgrades 100+ formulae/
  casks on every rebuild (slow, noisy). Consider false + explicit upgrades.

## Acceptance criteria

- [ ] Inventory captured of actually-installed tools (brew list, /Applications, PATH binaries)
- [ ] Diff produced: installed-but-undeclared vs declared
- [ ] Tailscale and 1Password declared in the appropriate layer
- [ ] emacs-plus migrated to nixpkgs `emacs` (d12frosted tap dropped); Doom re-verified
- [ ] Other discovered gaps declared (or consciously left ephemeral, with a note)
- [ ] `darwin-rebuild` completes clean with the additions

## Blocked by

- None - can start immediately
