# Org notes: a cohesive PARA storage + sync system across all devices

Status: DONE 2026-06-16 (desktop mesh + phone syncing; cleanup follow-ons noted)

## What to build

A single cohesive system for **file storage, syncing, and note-taking** across
macOS (hestia), NixOS (athena), and iOS (theseus), with the Synology NAS
(mnemosyne) as the always-on canonical sync node. The demoable tracer for the
Org goal: the same notes — *and the documents they link to* — open on every
device, links intact, placed/synced declaratively where possible.

This issue was re-designed from a greenfield "what do I need" pass (2026-06-16)
that superseded the earlier flat-denote / `~/notes` plan. See "Superseded
decisions" at the bottom.

## Requirements (derived from needs, not current tools)

1. **Capture fast** — dump a thought to an inbox from any device, phone included.
2. **Find a note later on the phone** — no emacs, no remembering a timestamp.
3. **Tasks / agenda (GTD)** — mostly desktop; phone does capture + read + the
   occasional checkbox / TODO-state toggle.
4. **Open a note and its linked docs anywhere, links intact.**
5. Cross-platform (macOS + NixOS + iOS), synced, reproducible.

Each requirement rules something out:
- Phone-findability (#2) needs **folders with readable names**, not denote's
  flat timestamp filenames → **PARA folders, not denote** as the organizer.
- Link integrity (#4) means a note's linked docs must be present wherever the
  note opens → **co-locate notes with their reference docs** in one synced tree.
  There is no "sync notes only" option that keeps links alive.

## Design (the cohesive system)

### Storage classes — three kinds of data, three sync strategies

| Class | What | Lives | Sync |
|-------|------|-------|------|
| **PARA tree** | notes + their *active* reference docs, co-located | home-root dirs (below) | Syncthing mesh (hestia + athena + NAS); phone via WebDAV |
| **Media library** | Calibre, photos, audio, video, big binaries | `~/media` and/or NAS shares | solved separately (follow-on) |
| **Cold archive** | dead PARA items, bulk old files | `~/archive` | Syncthing to desktops + NAS; NOT the phone |

### PARA tree layout — home-root, co-located, readable names

```
~/gtd/                       ← GTD control files (inbox/todo/someday/journal.org)
~/projects/<name>/...        ┐
~/areas/<name>/...           ├ Syncthing folders: gtd + 3 buckets, full mesh
~/resources/<name>/...       ┘ + phone via mnemosyne WebDAV
~/archive/...                ← desktops + NAS (not phone)
~/code/<repo>                ← unchanged; PARA "projects" link to repos here
~/media or NAS               ← big binaries, separate sync (follow-on)
```

**GTD control files live in `~/gtd` (option A, decided 2026-06-16):** Syncthing
syncs folders, not loose files, so the four control files can't sit at bare `~/`
without syncing all of `$HOME`. `~/gtd` gives them a tidy folder root; the
"short path at root" benefit was illusory (capture is keybind-driven, never
typed). `org-directory = ~/gtd`. PARA buckets are siblings, referenced by
absolute path in `org-agenda-files`.

- **Home-root PARA** (`~/projects`, `~/areas`, `~/resources`, `~/archive`): no OS
  default uses these names (defaults are Documents/Downloads/Desktop/Pictures/
  Music/Movies/Library/Public/.config) → no clobber on macOS or NixOS. Bonus:
  macOS iCloud only auto-syncs Desktop + Documents, so a home-root tree dodges
  iCloud entirely. Short ergonomic paths: `~/projects/<name>/<file>`.
- **Co-location:** a note sits in the same folder as the docs it references, so
  links (even relative) never break across machines or on the phone.
- **PARA = folders, not denote.** The folder is the organization. Cross-cutting
  concerns stay as inline org tags (`:work:`). **Denote is retired** (see #12).
- **`~/projects` vs `~/code`:** code repos stay in `~/code`; a coding effort's
  notes/docs live in `~/projects/<name>` and *link* to `~/code/<repo>`. Two
  locations, one bridge — same pattern as media. Named here to avoid confusion.

### PARA, correctly (taxonomy fix for the migration)

Existing `~/Documents/ref` conflates two PARA buckets:
- **Areas** (ongoing responsibility, no end): car, house, finances,
  identification, home, receipts → `~/areas/`
- **Resources** (topic of interest / future reference): dnd, climbing, gaming,
  books, penmanship, numismatics, woodworking, drums → `~/resources/`
- **Projects** (goal + an end): filecoin, scaffold-stellar, fangoat → `~/projects/`
- **Archive**: existing `~/Documents/archive/*` → `~/archive/`

### Sync — three legs

- **Desktops + NAS:** Syncthing mesh. NAS (mnemosyne) is the always-on canonical
  node so the two desktops stay in sync even when only one is on. Off-LAN rides
  **Tailscale**. Per-folder shares: `~/gtd`/`~/projects`/`~/areas`/`~/resources`
  ride the full mesh + phone; `~/archive` rides desktops + NAS but not the phone.
- **Phone (theseus):** **Plain Org → WebDAV server on the NAS, over Tailscale.**
  iOS Syncthing is too weak (foreground-only), and we're killing iCloud/Dropbox,
  so WebDAV is the phone's transport. Phone browses the whole PARA tree
  on-demand (pulls a note/PDF when tapped, no full local replication → also
  solves phone weight). Capture/edits write back to the NAS copy → Syncthing
  fans out to the desktops.
- **Media / cold storage:** separate, later. Big binaries → media library
  (Synology Photos / NAS shares / `~/media`), own sync policy. (`~/archive` is
  *not* media — it stays in the Syncthing mesh on desktops + NAS, just off-phone.)

### iOS client: Plain Org

Beorg is overkill for capture + read + occasional toggle. Plain Org does capture
templates and TODO-state cycling and is simpler. **One thing to verify before
committing:** that Plain Org's **WebDAV** support is solid (vs. only iCloud/
Dropbox being first-class). If it's weak, fall back to **organice** (a PWA, works
in any browser incl. NixOS, talks WebDAV); Plain Org is the nicer native option.

### Config placement

- `org-directory` + `org-agenda-files` set to the home-root PARA tree
  (`~/inbox.org` plus the bucket dirs, recursive or curated). Placed by
  home-manager, cross-platform (home-relative paths, no machine-specific path).
- Syncthing via HM `services.syncthing` in the shared `home.nix` (launchd on
  macOS, systemd on NixOS — both supported in HM 25.11). Declarative devices +
  folders, `overrideDevices`/`overrideFolders = true` (nix is source of truth).
- Tailscale: athena via nix (`services.tailscale.enable`), mnemosyne via the
  Synology Tailscale package. hestia + theseus already on the tailnet.
- mnemosyne Syncthing + WebDAV configured in DSM (outside nix).

## Slices

- **6a — PARA tree scaffolding + migration.** Create `~/inbox.org` +
  `~/{projects,areas,resources,archive}`. Migrate the real corpus out of Beorg's
  iCloud container (`~/Documents/notes` → `iCloud~...beorg/Documents/`) and the
  `~/Documents/{projects,ref,archive}` files into the new tree, splitting
  `ref` → `areas`/`resources` (taxonomy fix above). Drop the stale iCloud
  symlinks (`~/notes`, `~/Documents/notes`). Co-locate docs with their notes.
- **6b — Doom config.** Point `org-directory`/`org-agenda-files` at the new tree;
  set up capture → `~/inbox.org` + refile into PARA buckets; **remove denote**
  (folds into #12). Out-of-store config → no rebuild, just restart Emacs.
- **6c — Tailscale on the missing nodes.** athena via nix; mnemosyne via Synology
  pkg. Verify all four (hestia/athena/mnemosyne/theseus) on the tailnet.
- **6d — Syncthing declarative in `home.nix`.** Enable on both desktops; collect
  device IDs; declare the 3 devices + the bucket folders (override = true).
  `~/archive` shared to desktops + NAS (not the phone).
- **6e — mnemosyne.** Syncthing on the Synology (Docker via Container Manager if
  the model supports it — tracks upstream / version parity with nixpkgs-25.11
  desktops; else SynoCommunity), grab its device ID (feed 6d), share the tree as
  canonical. Stand up the **WebDAV server** for the phone leg.
- **6f — phone.** Install Plain Org, point it at NAS WebDAV over Tailscale; verify
  capture + read + a TODO toggle round-trips. (Verify WebDAV support first; else
  organice.)
- **6g — verify end-to-end.** Edit a note on hestia → appears on athena (relayed
  via mnemosyne, the other desktop off); open a note + its linked PDF on the
  phone; capture from the phone shows up on a desktop.

## Build progress

- **6a DONE 2026-06-16 (hestia).** PARA tree scaffolded at home root
  (`~/{projects,areas,resources,archive}` + GTD control files `~/inbox.org`,
  `~/todo.org`, `~/someday.org`, `~/journal.org`). Notes corpus migrated from
  Beorg iCloud container (28 .org, denote names → readable, co-located into
  buckets; near-empty home.org/notes.org skipped). Reference docs migrated from
  `~/Documents`: ref/{car,house,home,receipts,resume,climbing,penmanship,
  finances,identification} → areas/resources; loose resume/health PDFs +
  woodworking.org placed; filecoin .docx co-located into `~/projects/filecoin`;
  `~/Documents/archive` (1.7G) + ref/hickory (inactive LLC) → `~/archive`.
  **Copies/moves preserved Beorg originals** (backup until Syncthing verified).
  Deferred in `~/Documents`: ref/backup-codes (→ 1Password, security: plaintext
  2FA/Signal — must NOT enter synced tree), ref/{books,dnd,gaming} + calibre +
  Max 9 (media library follow-on). finances/identification confirmed sanitized →
  safe to sync.
- **6b DONE 2026-06-16 (hestia, doom/config.el).** `org-directory` → `~/`;
  `org-default-notes-file` → `~/inbox.org`; added `org-agenda-files` (control
  files + recursive `~/projects` + `~/areas`; resources/ excluded as reference;
  static-at-load, restart to pick up new files — dynamic glob deferred to #12).
  Denote block + keybind commented out (functional removal; packages.el line +
  `doom sync` removal tracked in #12). Out-of-store config → restart Emacs, no
  rebuild. Stale iCloud symlinks `~/notes` + `~/Documents/notes` removed (Beorg
  container backup intact: 30 .org).
- **6c DONE 2026-06-16.** Tailscale up on all four nodes (hestia, athena via nix
  `services.tailscale`, mnemosyne via Synology pkg, theseus). athena rebuilt via
  `rebuild`. Gotcha: hestia rebuild aborted on `brew bundle` — Homebrew now
  refuses untrusted taps and the bundle runs under sudo (root env) so user-level
  `brew trust d12frosted/emacs-plus` didn't apply. TEMP-disabled emacs-plus@30 +
  its tap in hosts/hestia/default.nix (already installed, cleanup="none" → not
  removed); proper fix (Emacs → nixpkgs) tracked in #10.
- **6d DONE 2026-06-16.** Device IDs collected (Syncthing v2.0.10; `--device-id`
  flag gone in v2 → read from config.xml `<device id>`): hestia 4KBA7WY…,
  athena AHKWQII…, mnemosyne ZHAZDXG…. Filled `home.nix services.syncthing.settings`
  — 3 devices + 5 folders (gtd/projects/areas/resources/archive), all shared to
  the 3 peers, paths via `${config.home.homeDirectory}`. `darwin-rebuild build`
  validated; committed+pushed; rebuilt hestia + athena.
- **6e DONE 2026-06-16.** mnemosyne Syncthing via Container Manager (official
  `syncthing/syncthing` image, `network_mode: host`, `user: "1026:100"` — the
  `nas` uid; PUID/PGID env wasn't honored → used `user:`. config vol
  /volume1/docker/syncthing, data /volume1/para→/data, STGUIADDRESS 0.0.0.0:8384).
  Accepted hestia/athena devices + the 5 folder shares in its GUI, paths /data/<name>.
- **6g (desktop side) DONE 2026-06-16.** Sync verified across hestia/athena/mnemosyne.
- **6f + 6g DONE 2026-06-16.** Plain Org does NOT support WebDAV (nor do Beorg/
  organice end up needed). Solution: DSM **WebDAV Server** (HTTPS :5006) exposes
  the `para` share; iPhone uses **iOS Files' native "Connect to Server"** WebDAV
  to `https://100.105.93.7:5006` (mnemosyne's stable Tailscale IP), and **Plain
  Org reads/writes org files through the Files picker**. Phone↔desktop sync
  verified (WebDAV writes to /volume1/para → Syncthing fans out). No CORS/PWA
  rabbit hole, no iCloud. archive simply not added on the phone.
- **#06 COMPLETE.** Follow-on cleanups (separate): delete Beorg iCloud originals
  + retire Beorg/iCloud once happy with the new flow; ref/backup-codes → 1Password
  (security); media library (ref/{books,dnd,gaming}+calibre+Max9); emacs-plus →
  nixpkgs to drop the brew tap (#10).

## Acceptance criteria

- [x] Notes + their active reference docs live co-located in one syncable PARA tree
- [x] Editing/capturing on one device appears on the others after sync
- [x] Linked docs open intact on every device, including the phone
- [x] `~/archive` syncs to the desktops + NAS, but not the phone
- [x] Org config (org-directory, agenda files, capture/refile) placed by
      home-manager, cross-platform, no hardcoded machine-specific paths
- [x] Phone (Plain Org via Files WebDAV/Tailscale) does capture + read + TODO toggle

## Follow-on (out of scope here)

- **Media library**: Calibre / photos / audio / video → own location + sync
  (Synology Photos / NAS shares / `~/media`). Separate issue.
- **Denote retirement** detail + broader org simplification → #12.

## Superseded decisions (do not re-adopt)

- ~~`org-directory = ~/notes`, flat denote corpus, PARA as keywords only.~~
  Replaced by home-root PARA folders + co-location (phone-findability + link
  integrity drove this). The earlier `doom/config.el` `org-directory → ~/notes`
  edit (6a, old) points at a near-empty iCloud CloudDocs folder — to be redone
  in 6b. The real corpus is in Beorg's iCloud container, migrated in 6a (new).
- ~~iCloud as the sync layer (from the claude.ai project summary).~~ iCloud can't
  reach NixOS; retired in favor of Syncthing + Tailscale + NAS WebDAV. Retiring
  Beorg = retiring iCloud as the notes transport.

## Infra facts

NAS = mnemosyne (Synology). iPhone = theseus. Droplet = hephaestus (DigitalOcean,
internet-facing, not in flake). Desktops = hestia (macOS), athena (NixOS,
192.168.1.3). Earlier nix work already in repo on `feat/nixos-host`:
`hosts/athena/default.nix` `services.tailscale.enable = true` (6c); `home.nix`
`services.syncthing` enabled with `override*=true` + EMPTY device/folder
placeholders (6d, to fill). Pending /simplify cleanups: flake.nix
system-redundancy removal, home.nix dead zimfw fn.

## Blocked by

- #04 widen home-manager passthrough (done)
- #05 basic NixOS config (done)
- Build needs home/LAN access to mnemosyne + athena.
