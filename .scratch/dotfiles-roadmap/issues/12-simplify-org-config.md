# Simplify org-mode config to reduce friction and encourage use

Status: mostly-absorbed-by-#07 (2026-06-16) — pending only daily-use confirmation

## Absorbed into #07 (2026-06-16)

The decided simplifications were implemented while building #07 (user chose "build
#07 lean, fold #12 in"): denote retired (config.el block + packages.el +
`doom sync`); lean keyword set `TODO NEXT WAIT | DONE CLOSED` (dropped MEETING
keyword + `[ ]/[-]/[X]` checkbox seq → native list checkboxes); capture trimmed to
todo/note/journal (dropped meeting/food); `org-refile-targets` → PARA buckets;
archive split (task datetree → `~/archive/gtd-archive.org`, project = fs-move via
`zf/archive-project`); prepend on capture+refile; which-key labels fixed. See #07
build progress. **Only remaining criterion: user confirms the daily flow feels
lower-friction after living with it.**

## What to build

Review the org-mode sections of `doom/config.el` (633 lines total) and simplify
them to cut day-to-day friction, so the user reaches for org more often. Guided
by the distilled insights from #11. Focus on the high-traffic surfaces: capture
templates, agenda views, refile/archive targets, and keybindings.

End-to-end: a leaner, frictionless org setup that the user actually wants to use
daily, with config still cross-platform (no machine-specific assumptions) and
placed by home-manager. Relates to #06 (org sync) and #07 (org workflow).

## Decided simplifications (from #11 distillation + #06 design, 2026-06-16)

- **Retire denote.** In the co-located PARA-folder model (#06) the folder *is*
  the organization; timestamp IDs + a tag-search tool add friction and break
  phone-findability. Remove the `(use-package! denote ...)` block (config.el
  ~177-184), the `denote` keybind (`SPC n d`), and `(package! denote)`
  (packages.el:60). Cross-cutting concerns become inline org tags (`:work:`).
- **Capture → `~/inbox.org`, refile into PARA buckets.** Set `org-refile-targets`
  to the home-root PARA tree so processing the inbox is one keystroke per item.
- **GTD keyword set:** lean toward `TODO NEXT WAITING | DONE CANCELLED` (fewer
  states = less friction); confirm against current `org-todo-keywords`.
- Trim capture templates to the few that actually get used (todo / note /
  journal); audit the rest.

## Acceptance criteria

- [ ] Org sections of config.el reviewed; friction points identified
- [ ] Capture, agenda, refile/archive, and keybindings simplified
- [ ] Denote removed (config.el + packages.el); PARA folders organize instead
- [ ] Changes reflect the actionable items from #11
- [ ] Config remains cross-platform and home-manager-placed
- [ ] User confirms the daily workflow feels lower-friction

## Blocked by

- #11 Import claude.ai org-mode project insights into the repo
