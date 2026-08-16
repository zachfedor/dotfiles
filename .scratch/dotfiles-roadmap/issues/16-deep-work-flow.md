# Deep work flow: focus sessions, distraction capture, work-life boundaries

Status: needs-triage (brainstormed 2026-07-04)

## Origin

Sparked by a repo-wide ideation session: "what's next after #07 Org workflow, #08
Flavours, #13 Home server, #14 Hyprland?" — the user named a new axis not yet
tracked in the roadmap: **work vs play management** — using the existing dotfiles
stack to reduce procrastination, analysis paralysis, and distractions while making
it easier to "turn off" for healthy work-life balance.

## Problem space (user's own words)

- "analysis paralysis" — too many choices, hard to commit to one task
- "hard to keep working" — gets distracted easily, not just at session boundaries
- "GTD is mostly aspirational" — tools need to help form habits that don't exist yet
- "consolidation" — any solution should live inside org-mode + PARA, not fragment
  into a separate system

## Ideated approaches

### Approach A: CLI orchestrator (`dw` command)

A `scripts/dw` bash/zsh script that calls existing tools (pomo, mjadb, hammerspoon
timer, terminal-notifier) to start/stop focus blocks, block distractions, switch
themes.

**Pros:**
- Quick to build (~60-80 lines)
- Works without Emacs running
- Uses familiar shell idioms

**Cons:**
- Separate state dir (`~/.local/share/dw/`) — fragments from PARA consolidation
- Relies on legacy scripts the user feels aren't "sacred"
- Asks "what are you working on?" — **adds a decision**, doesn't solve analysis
  paralysis
- Site-blocking (mjadb) treats symptoms, not the habit
- macOS-only without extra work

### Approach B: Emacs-native focus workflow

Four Emacs commands in `doom/config.el`. A single org file `~/gtd/focus.org`
(auto-managed, syncthing-replicated). Timer via `org-timer`, clocking via
`org-clock`, distraction capture as org items. Zero bash scripts.

**Key commands:**
- Morning (`SPC n m`): pick one NEXT from agenda → clock in + start timer
- Distraction (`SPC n d`): log urge in `* Distractions` under current session,
  return to buffer (2-second capture, no context loss)
- Evening (`SPC n E`): clock out, reflect, set tomorrow's #1
- Stats (`SPC n S`): summary of sessions + distraction patterns

**Pros:**
- Everything in org-mode under PARA (`~/gtd/`), syncthing-replicated
- Distraction logging > blocking: naming the urge breaks autopilot, log reveals
  patterns without guilt
- Morning ritual removes the "what to work on" decision — shows exactly one: the
  top NEXT from agenda
- Cross-platform (Emacs is the same everywhere)
- No new dependencies

**Cons:**
- Requires Emacs running for full experience
- Distraction capture needs a global keybind to work from any app (Emacs must be
  backgrounded or running as daemon)
- No visual boundary (theme swap) unless added later
- Still assumes the user will engage with the morning ritual — habit not formed

### Approach C: Minimal timer-only (no org, no capture)

A single `dw` script that does only: start timer, show end time, notify on
completion. No task prompt, no distraction capture, no logging.

**Pros:**
- Lowest possible activation energy
- Impossible to "do it wrong"

**Cons:**
- No feedback loop, no habit scaffolding, no integration with existing PARA system
- Doesn't address analysis paralysis, distraction, or work-life boundaries at all

## Points of friction (unresolved)

1. **Org integration vs low barrier.** Approach B is more aligned with the
   consolidation value, but it increases activation energy (Emacs must be running,
   agenda must have NEXT items, the user must do a morning ritual). Approach A is
   faster to start but fragments the system. The right balance isn't obvious.

2. **Distraction capture mechanism.** The user wants to log distractions instead of
   following through on them. But from a non-Emacs app (browser, Slack, terminal),
   a keystroke to "capture this urge" likely needs an OS-level hotkey. On macOS
   this could be Hammerspoon (global hotkey → append to focus.org). On Linux this
   could be a system-wide shortcut. The cross-platform story is unclear.

3. **Habit formation vs system design.** The user explicitly stated "I'm trying to
   create these tools to help me with habits I haven't formed yet." This means the
   system should be designed for someone who *wants* to do morning reflections and
   distraction logging but currently doesn't. There's tension between building
   something complete (that could work once habits form) vs something minimal
   (that lowers the barrier to starting the habit).

4. **Bounded commitment vs deep work.** The user wants bounded commitments (25min
   timer) for "deep work" but also wants to "keep working" when in flow. These
   are slightly in tension — a hard stop from the timer can interrupt flow. Should
   the timer be an alarm (hard stop), a suggestion (gentle nudge), or something
   that auto-extends if the user is mid-thought?

5. **How separate from the existing org workflow (#07)?** The deep-work system
   overlaps significantly with what #07 already provides (capture, agenda, clocking,
   refile). Is this a new concern, or an extension of #07? If the latter, should it
   be folded into #07 rather than tracked as a separate initiative?

## Next-actions for exploration

- [ ] Spend a week using just `org-timer` + `org-clock` manually. What friction
      surfaces? Where do you actually get stuck?
- [ ] Try a paper-based distraction log (notepad beside keyboard) for 3 days. Does
      the act of writing interrupt the urge? What categories emerge?
- [ ] Define what "winning" looks like: after X sessions, what would make you feel
      the system is working?
- [ ] Revisit after #07's daily flow feels "lived-in" enough to identify where the
      real gap is.

## Related

- #07 Org workflow (capture/agenda/refile) — foundation this would build on
- #14 Hyprland WM — possible venue for Linux-side focus-mode integration
- #08 Flavours/ricing — could provide visual work/personal boundary later
- <did not converge on implementation>
