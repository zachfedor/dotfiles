# Import claude.ai org-mode project insights into the repo

Status: DONE 2026-06-16 (summary imported + distilled into #06/#12; all criteria met)

## What to build

There is an active project on claude.ai with multiple chats discussing org-mode
workflow and config changes. Claude Code cannot reach that project directly, so
this slice is HITL: the user exports/pastes the relevant chat content, and we
distill it into a single actionable notes document living in the repo.

The output is a concrete, deduplicated list of desired `config.el` changes
(capture templates, agenda views, keybindings, workflow tweaks) — not a dump of
the chats. This feeds the simplification work in #12.

## Progress (2026-06-16)

The claude.ai project summary was exported as `dotfiles-project-summary.md` and
reconciled against the real setup + roadmap in a working session. Findings:

- **Storage / sync / note-taking** half → distilled into the re-designed **#06**
  (cohesive PARA system: home-root co-located tree, Syncthing+Tailscale+NAS
  WebDAV, drop denote). The summary's "settled iCloud" decision was stale
  (predated the NixOS box) and is overridden there.
- **Org workflow / config** half (capture templates, agenda views, keywords,
  keybindings, GTD states `TODO/NEXT/WAITING/DONE/CANCELLED`, denote-retirement)
  → feeds **#12** simplification.

Audit notes worth keeping:
- Real corpus lives in Beorg's iCloud container (`~/Documents/notes` →
  `iCloud~com~appsonthemove~beorg/Documents/`), ~20 flat keyword-tagged denote
  files + agenda files (`inbox/todo/work/someday/journal`). `org-directory =
  ~/notes` (config.el) points at the *empty* CloudDocs folder — bug, fixed in #06.
- Org-roam already absent from config (summary's "strip org-roam" = no-op).
- VimWiki `~/Dropbox/wiki/` → org via pandoc remains a low-priority backlog migration.

The summary file itself was removed after this distillation (content captured in
#06 + #12).

## Acceptance criteria

- [x] Relevant claude.ai content gathered (summary export)
- [x] Content distilled into the repo (folded into #06 and #12, not a separate dump)
- [x] Items are concrete and actionable: storage→#06, config→#12
- [x] Duplicates/contradictions resolved (stale iCloud + ~/notes decisions overridden)

## Blocked by

- None - can start immediately (requires user to supply the chat content)
