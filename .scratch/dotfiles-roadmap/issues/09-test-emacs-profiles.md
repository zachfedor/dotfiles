# Test other Emacs profiles via Doom native profiles

Status: needs-triage

## What to build

With Chemacs2 dropped in favour of Doom's native profile system (ADR-0002),
experiment with additional Emacs configs as opt-in profiles defined in
`doom/profiles.el`, launched via `emacs --profile NAME`. Lowest priority —
exploration, not a blocker for anything. The daily-driver global profile must
stay untouched and stable.

## Acceptance criteria

- [ ] At least one extra profile defined in `doom/profiles.el` (templates for the
      old `scratch`/`kickstart`/`nano`/`prelude` configs are stubbed there)
- [ ] `emacs --profile NAME` launches it cleanly, packages sandboxed under
      `$XDG_DATA_HOME/doom/NAME/`
- [ ] Plain `emacs` (global profile) is unaffected
- [ ] Notes recorded on which experiment to keep/drop

## Blocked by

- #01 Diagnose & fix Emacs startup (Chemacs removed / framework relocated there)
