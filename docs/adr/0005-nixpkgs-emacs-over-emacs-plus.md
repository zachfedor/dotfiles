# nixpkgs `emacs30` instead of the `emacs-plus` Homebrew tap

Emacs on macOS is provided by the nixpkgs `emacs30` package (Cocoa/NS build) via
`home.nix`, not by the `d12frosted/emacs-plus` Homebrew tap. This brings Emacs
under declarative Nix management (parity with athena's `emacs30-pgtk`), and —
critically — removes the third-party tap that broke `darwin-rebuild`: Homebrew
refuses to load formulae from untrusted taps, and `brew bundle` runs under sudo,
so a user-level `brew trust d12frosted/emacs-plus` does not apply. The trade-off
is losing emacs-plus's mac-specific patches/polish; if the vanilla NS build feels
rough in daily use, `emacs30-macport` is the fallback (mac-only, diverges from
athena). Doom itself stays hand-managed (ADR-0002); after switching, run
`doom sync` + `doom env`.

## Considered and rejected

- **Keep emacs-plus, fix the tap trust** — the sudo/untrusted-tap conflict is
  structural, not a one-time glitch; re-adding the tap re-breaks the rebuild.
  Do not reintroduce `d12frosted/emacs-plus`.
