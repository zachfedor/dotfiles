# Audit hestia installed tools/programs → nix config

Status: DONE 2026-06-28 (grill-with-docs audit session) — pending only the user's
runtime confirmation of colima + `doom doctor`. Full verdict ledger:
`10-audit-decisions.md`. Decisions captured in ADR-0004/0005/0006 + CONTEXT.md.
Font-cask reconciliation split out to its own issue (#14).

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

- [x] Inventory captured of actually-installed tools (brew list, /Applications, PATH binaries)
- [x] Diff produced: installed-but-undeclared vs declared (`10-audit-decisions.md`)
- [x] Tailscale (nix-darwin service) and 1Password (cask + `_1password-cli`) declared
- [x] emacs-plus migrated to nixpkgs `emacs30` (d12frosted tap dropped, ADR-0005); Doom re-synced
- [x] Other discovered gaps declared / cut / left as vendored (with notes in ledger + new-host.md)
- [x] `darwin-rebuild` completes clean with the additions (after anki→Linux-only fix)
- [~] Containers (colima) + `doom doctor` vterm — pending user runtime confirmation

## Blocked by

- None - can start immediately
