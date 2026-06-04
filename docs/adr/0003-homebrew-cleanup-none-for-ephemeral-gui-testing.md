# Homebrew `cleanup = "none"` to preserve ephemeral GUI-app testing

## Context

The nix-darwin `homebrew` module manages GUI casks declaratively, but unlike the
Nix store it drives the *same* Homebrew used for manual installs. Its
`onActivation.cleanup` setting controls reconciliation: `"zap"` uninstalls
anything not declared in the flake on every rebuild; `"none"` leaves undeclared
packages alone.

The owner's mental model (correct for Nix proper): declared = permanent, ad-hoc =
ephemeral test that doesn't touch the managed set. `"zap"` violates this — a
`brew install --cask foo` to try something would be destroyed by the next
unrelated `darwin-rebuild`.

## Decision

Set `homebrew.onActivation.cleanup = "none"`. Rebuilds never auto-uninstall brew
packages. Reconciliation is manual and on-demand via `brew bundle cleanup`
(dry-run by default; `--force` to prune), fed the flake's generated Brewfile.

Testing workflow, split by tool type:
- **CLI tools** — `nix shell nixpkgs#pkg` / `nix run nixpkgs#pkg` for one-offs
  (ephemeral, nothing persists); add to `home.packages` to keep.
- **GUI apps** — `brew install --cask app` ad-hoc (Nix can't run `.app`s on macOS
  well); survives rebuilds under `cleanup = "none"`; add to `homebrew.casks` to
  make permanent.

## Why

- Preserves the Nix-like "declared = permanent, ad-hoc = ephemeral" model for GUI
  apps, which `"zap"` would break.
- Avoids destructive surprises: an unrelated rebuild can never silently uninstall
  an app you're trialing.
- Keeps a clean escape hatch for tidiness (manual `brew bundle cleanup` with a
  preview) without coupling pruning to every activation.

## Consequences

- Removing a cask from the flake list does **not** uninstall it — that's manual,
  consistent with the brew→nix migration rule in ADR-0001 (verify nix copy wins on
  PATH, then `brew uninstall`).
- Brew can drift from the declared set over time; `brew bundle cleanup` (dry-run)
  is the periodic check. Documented in README.
