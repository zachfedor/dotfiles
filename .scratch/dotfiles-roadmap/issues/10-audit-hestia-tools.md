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

## Acceptance criteria

- [ ] Inventory captured of actually-installed tools (brew list, /Applications, PATH binaries)
- [ ] Diff produced: installed-but-undeclared vs declared
- [ ] Tailscale and 1Password declared in the appropriate layer
- [ ] Other discovered gaps declared (or consciously left ephemeral, with a note)
- [ ] `darwin-rebuild` completes clean with the additions

## Blocked by

- None - can start immediately
