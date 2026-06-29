# Per-project devShells over global toolchains

Language runtimes, version managers, linters, LSP servers, and dev databases are
not installed globally; each project declares its own pinned toolchain in a
`flake.nix` devShell, loaded on `cd` via `.envrc` (`use flake`) + nix-direnv.
This was chosen over runtime version managers (pyenv/rbenv/n) and over `mise` for
reproducibility and cross-machine parity — a project's toolchain is pinned with
the project, not drifting in a global profile. Global packages are deliberately
limited to a thin cross-project fallback (`python3`, `nodejs`) plus ad-hoc CLIs
(`gh`, `jq`, `wget`); everything language- or version-specific lives in a devShell
(rust, clojure, go, PHP, the QMK keyboard-firmware toolchain, Postgres/MySQL/Redis).

## Consequences

Emacs needs explicit help to see devShell toolchains, because GUI Emacs inherits
no shell environment. Two layers cover this:

1. **Global baseline — `doom env`.** Snapshots the login-shell `PATH`/env into
   `~/code/doomemacs/.local/env`, which GUI Emacs loads at startup (so it sees the
   Nix profile, Homebrew, etc.). It is a *snapshot* — re-run `doom env` whenever
   the global PATH changes (e.g. after editing `home.nix`).
2. **Per-project — Doom `:tools direnv` (envrc).** Applies each buffer's `.envrc`
   devShell environment to that buffer's `exec-path`, so `:lang` LSP servers
   (rust-analyzer, clojure-lsp, …) are found from the project's devShell rather
   than a global install. This module is **required** whenever a `+lsp` language
   module is enabled but its toolchain is devShell-only — without it, LSP silently
   fails to launch.
