# Arr suite: automated Usenet media acquisition on mnemosyne

Status: ready-to-build (proposed 2026-10-04)

## What to build

Automated media acquisition feeding the existing Jellyfin library: **Prowlarr**
(indexer hub) → **Sonarr** (TV) + **Radarr** (movies) → **SABnzbd** (Usenet
download client), running as a second Container Manager compose project on
mnemosyne, next to Jellyfin. Optional niceties: **Bazarr** (subtitles),
**Recyclarr** (TRaSH quality profiles). Decision captured in **ADR-0010**.

## Placement (see ADR-0010)

- **mnemosyne.** Always-on; already the Container Manager host; co-located with the
  library (mandatory for hardlink/atomic-move imports).
- **NOT argus** — 1GB Pi on SD, deliberately minimal/security-sensitive (not a
  Syncthing peer, #13); SQLite + download I/O would shred the card.
- **NOT athena** — asleep by default (#18/ADR-0007); a 24/7 stack defeats that.
  The Arr suite is acquisition only and is orthogonal to the WoL transcode hatch.

## Filesystem & the hardlink rule (the thing people get wrong)

Sonarr/Radarr import by **hardlink or atomic move**, which only works when the
download dir and the media live on **one volume, exposed under one mount**. So mount
`/volume1/Media Library` as a single `/data` into every container — NOT separate
`/downloads` + `/media` mounts (that silently reverts imports to slow full copies and
breaks seeding-less instant imports).

Live NAS layout (verified 2026-10-04) + the one dir to add:

```
/volume1/Media Library/        ← mount as /data into all Arr + SAB containers
├── downloads/                 ← ADD: {complete,incomplete}  (SAB writes here)
├── Shows/                     ← Sonarr root folder = /data/Shows
├── Movies/                    ← Radarr root folder = /data/Movies
├── Books/   Music/  Photos/  games/   (existing; untouched)
├── @eaDir/  #recycle/         (Synology internal; ignore)
```

Jellyfin already reads this tree and its libraries point at the specific `Shows/` and
`Movies/` subfolders, so it never scans the `downloads/` sibling. (Semantically odd to
nest `downloads/` under "Media Library", but it's the minimal change that keeps
everything on one volume+mount; a cleaner `/volume1/data` root is a future restructure,
not worth a Jellyfin remount now.)

## Compose sketch (sanitized template — NOT committed to the repo)

Container Manager → Project → create `arr` with this `docker-compose.yml`. Lives only
on the NAS (carries API keys later), matching the Jellyfin/Syncthing precedent.
`network_mode: host` mirrors Jellyfin and makes each UI reachable on the host's
`tailscale0` for free. PUID/PGID = the `nas` account (`1026:100`); LinuxServer images
DO honor PUID/PGID (unlike the raw `jellyfin` image, #13, which needed `user:`).

```yaml
# /volume1/docker/arr/docker-compose.yml   (NAS-only; do not commit)
x-env: &env
  PUID: "1026"          # nas uid
  PGID: "100"           # users gid
  TZ: "America/New_York"  # adjust
  UMASK: "022"

services:
  prowlarr:
    image: lscr.io/linuxserver/prowlarr:latest
    container_name: prowlarr
    network_mode: host                       # UI :9696
    environment: *env
    volumes:
      - /volume1/docker/prowlarr:/config
    restart: unless-stopped

  sonarr:
    image: lscr.io/linuxserver/sonarr:latest
    container_name: sonarr
    network_mode: host                       # UI :8989
    environment: *env
    volumes:
      - /volume1/docker/sonarr:/config
      - /volume1/Media Library:/data         # single mount → hardlinks work
    restart: unless-stopped

  radarr:
    image: lscr.io/linuxserver/radarr:latest
    container_name: radarr
    network_mode: host                       # UI :7878
    environment: *env
    volumes:
      - /volume1/docker/radarr:/config
      - /volume1/Media Library:/data
    restart: unless-stopped

  sabnzbd:
    image: lscr.io/linuxserver/sabnzbd:latest
    container_name: sabnzbd
    network_mode: host                       # UI :8080 (free on NAS)
    environment: *env
    volumes:
      - /volume1/docker/sabnzbd:/config
      - /volume1/Media Library:/data         # complete → /data/downloads/complete
    restart: unless-stopped

  # --- optional; omit either to save RAM on the stock-4GB DS923+ ---
  bazarr:
    image: lscr.io/linuxserver/bazarr:latest
    container_name: bazarr
    network_mode: host                       # UI :6767
    environment: *env
    volumes:
      - /volume1/docker/bazarr:/config
      - /volume1/Media Library:/data
    restart: unless-stopped
```

**Recyclarr** (TRaSH quality profiles): run **on-demand**, not as a persistent
container (saves RAM). `docker run --rm -v /volume1/docker/recyclarr:/config
ghcr.io/recyclarr/recyclarr sync` after editing its config; re-run when profiles drift.

## Wiring (in the UIs, after first start)

1. **SABnzbd** — add your Usenet provider (server/SSL/connections), set folders:
   complete `/data/downloads/complete`, incomplete `/data/downloads/incomplete`.
   Create categories `tv` and `movies`.
2. **Prowlarr** — add your indexer(s); add Sonarr + Radarr as Apps (sync pushes
   indexers to them); add SABnzbd as the download client (it propagates down).
3. **Sonarr** — root folder `/data/Shows`, download client SABnzbd (category `tv`).
   **Radarr** — root folder `/data/Movies`, SABnzbd (category `movies`).
4. **Jellyfin** — no change; it already serves `Shows/` and `Movies/`. New files land
   via hardlink and appear on the next scan.

## Synology specifics

- **ACL (reuse the #13 fix).** Containers run as uid 1026 and do NOT inherit `nas`'s
  supplementary groups, so Jellyfin-style read ACLs aren't enough — the Arr apps must
  **write** (import/rename). Add an explicit `user:nas allow:rwx` ACE on
  `/volume1/Media Library`, recursive: File Station → Properties → Permission → add
  `nas` with read+write → "Apply to this folder, sub-folders and files".
- **RAM.** Stock 4GB, shared with Jellyfin. Core four (Prowlarr/Sonarr/Radarr/SAB) are
  fine; SABnzbd **par2 repair + unpack** is the spike to watch (keep Bazarr optional,
  Recyclarr on-demand). A RAM stick is the obvious relief valve if it gets tight.
- **Ports.** SABnzbd `:8080` is free on the NAS (verified 2026-10-04), so its default
  stands under `network_mode: host`. Others are conflict-free too: Prowlarr 9696,
  Sonarr 8989, Radarr 7878, Bazarr 6767, Jellyfin 8096.

## Remote access

Tailnet-only, no port-forwarding. Bookmark each UI by MagicDNS:
`http://mnemosyne.<tailnet>.ts.net:{9696,8989,7878,8080,6767}` (matches the existing
net-agnostic bookmark convention from #13). All provider/indexer/download traffic is
outbound from the NAS; nothing inbound beyond the tailnet.

## Acceptance criteria

- [ ] `arr` compose project up in Container Manager alongside `jellyfin`
- [ ] `/volume1/Media Library/downloads/{complete,incomplete}` created; `user:nas rwx`
      ACE applied recursively to `Media Library`
- [ ] SABnzbd connected to the Usenet provider; a test NZB completes + unpacks
- [ ] Prowlarr syncs indexer(s) + SAB down to Sonarr/Radarr
- [ ] A Sonarr grab **hardlinks** into `/data/Shows` (verify link count > 1, not a copy)
      and Jellyfin shows the new item
- [ ] All UIs reachable over the tailnet by MagicDNS; nothing exposed to the internet
- [ ] Sanitized compose kept on the NAS only; no API keys in the repo

## Related

- **ADR-0010** — this decision (placement / Usenet-only / tailnet-scoped).
- **ADR-0007** + **#13** — Jellyfin on mnemosyne; the library this feeds; the reusable
  Synology container-ACL gotcha.
- **#18** — WoL/athena sleep posture that rules athena out as an always-on host.
- Future: torrents = additive qBittorrent-behind-Gluetun in this same project (its own
  ADR if adopted); `/volume1/data` root restructure if the nested `downloads/` ever grates.
