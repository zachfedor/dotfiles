# Org workflow: capture / agenda / refile, cross-platform

Status: in-progress (core built 2026-06-16, hestia; built lean per #12 — see below)

## Build progress

**2026-06-16 (hestia, doom/config.el + packages.el).** Built #07 in #12's lean
direction (user: "build lean, fold #12 in"; config treated as fluid/experimental).

- **Keywords** → lean set `TODO NEXT WAIT | DONE CLOSED` (was: 3 sequences incl.
  MEETING keyword + `[ ]/[-]/[X]` checkbox sequence). Subtask checklists now use
  native org list checkboxes (`- [ ]`, `C-c C-c`) — outside the keyword set.
  MEETING → inline `:meeting:` tag. keyword-faces trimmed (NEXT/WAIT/CLOSED).
- **Capture** trimmed to `t` todo / `n` note / `j` journal (all → ~/gtd/inbox.org
  except journal datetree). Dropped `m` meeting + `f` food log (food.org never
  existed). note template gained `%U` timestamp.
- **Refile** (new): `org-refile-targets` = `zf/org-refile-targets` fn returning
  gtd todo/someday + recursive projects/ + areas/ .org, `:maxlevel 1`. Function
  form → new bucket files picked up without restart. resources/ excluded
  (reference); inbox/journal excluded (refile OUT of them). `org-refile-use-
  outline-path 'file`, `…complete-in-steps nil`, `…allow-creating-parent-nodes
  'confirm`. (Closes the stale "look up refile targets" TODO in inbox.org.)
- **Agenda** (new): `org-agenda-custom-commands` n=NEXT actions, w=Waiting,
  r=Review block (day agenda + NEXT + WAIT). `org-agenda-files` unchanged.
- **Stuck projects** (new): file-based, NOT org's heading-level `org-stuck-
  projects`. A project = a file under `~/projects`; stuck = no `NEXT` line in it.
  `zf/stuck-projects` pops a buffer of offending files as links (`zf/stuck-
  project-files` helper). User chose file-model over a `:project:` tag.
- **Archive** (decided): two distinct ops.
  - *Task-level* `org-archive-subtree` (`SPC m A`) → `org-archive-location` =
    `~/archive/gtd-archive.org::datetree/` — one dated done-log in the cold
    bucket (off phone), gtd files stay lean. (Was in-file `::datetree/`.)
  - *Project-level* = filesystem move via new `zf/archive-project`: moves the
    current buffer's top dir under `~/projects` → `~/archive/<YYYY-MM>_<name>/`
    (kills its buffers, confirms, date-prefix matches existing archive naming).
    Archiving thus also drops the project off the phone (archive not phone-shared).
- **Keys** (`SPC n`): `n` inbox, `r` review agenda, `R` review/stuck projects,
  `A` archive project. (User renamed descs.) which-key labels: doom's key-based
  replacements override `map!` `:desc` in the popup → re-registered ours via
  `which-key-add-key-based-replacements` so labels match.
- **Prepend**: `:prepend t` on todo+note (captures → top of inbox) +
  `org-reverse-note-order t` (refiles also prepend). Closes the stale "append or
  prepend" TODO in inbox.org. `%a` annotation link kept on both templates.
- **Denote** fully retired: commented block removed from config.el, `(package!
  denote)` removed from packages.el, `doom sync` run (orphan build artifact
  remains; `doom gc` optional). Folds #12's denote-removal item.

Out-of-store config → **restart Emacs** to load (no rebuild). Parens validated
(`check-parens` in elisp-mode); doom faces verified to exist.

**Open / revisit:** verify on athena (restart Emacs there too); first real archive
will create `~/archive/gtd-archive.org` — add it to the archive Syncthing share if
it should propagate. #12 now largely absorbed — remaining: final friction review +
confirm the daily flow feels right after living with it.

## What to build (original)

## What to build

The ongoing, highest-value effort: streamline the actual note-taking workflow
on top of the portable foundation from #06 — capture templates, agenda views,
refile targets, and any keybindings. Continual; expect to revisit. Keep all
config cross-platform (no machine-specific assumptions).

## Acceptance criteria

- [ ] Capture templates defined and working on both machines
- [ ] Agenda views configured against the synced org files
- [ ] Refile targets and archive locations work cross-platform
- [ ] Workflow config lives in the repo and is placed by home-manager

## Blocked by

- #06 Org notes sync + open on both machines
