# Media library: home + sync for big binaries (PARA follow-on)

Status: needs-triage

## What to build

The #06 cohesive-storage design split data into three classes. #06 finished the
**PARA tree** (notes + small active reference docs, Syncthing mesh + phone) and
its **cold archive** (`~/archive`, desktops + NAS). This issue handles the third
class: the **media library** — large binaries that are neither notes nor active
note-linked reference and shouldn't ride the phone-bound Syncthing tree.

Give these a clear home and an appropriate sync/backup policy (NAS-canonical,
on-demand over Tailscale; not replicated to the phone), so the PARA tree stays
small and fast.

## What's deferred here (left in place during #06)

Still sitting in `~/Documents` on hestia, intentionally not migrated:

- `ref/books` (~1.8G) — ebooks / large PDFs
- `ref/dnd` (~1.6G) — battlemaps, large game assets (the *notes* are already in
  `~/resources/dnd`; only the bulk assets remain)
- `ref/gaming` (~1.2G) — game media (notes already in `~/resources/gaming`)
- `calibre/` (~20M) — Calibre ebook library
- `Max 9/` — Max/MSP app library

Also worth pulling in: photos / audio / video if they want a unified media home.

## Open questions (triage)

- **Location:** a `~/media/{books,games,...}` tree on the desktops, or live only
  on the NAS (mnemosyne) and reach over Tailscale on demand?
- **Sync mechanism:** a separate Syncthing folder (desktops + NAS, NOT phone),
  Synology-native (Synology Photos / shared folders mounted over Tailscale), or
  a mix per media type.
- **Cherry-pick:** any small dnd/gaming assets worth co-locating back into the
  `~/resources/*` notes folders (so they ride the synced tree + phone)?
- **Photos** specifically may want Synology Photos (mobile upload) rather than
  Syncthing — decide separately from books/games.

## Acceptance criteria

- [ ] Media library has a defined home (NAS-canonical and/or `~/media`)
- [ ] Big binaries removed from `~/Documents`; nothing note-linked left dangling
- [ ] Sync/backup policy set per media type; phone not bloated
- [ ] Any small note-linked assets cherry-picked into `~/resources/*`

## Blocked by

- #06 (done) — established the storage classes this follows from
