# Arr suite on mnemosyne, co-located with the library, Usenet-only, tailnet-scoped

The media-acquisition stack (Prowlarr + Sonarr + Radarr, with SABnzbd as the
download client and Bazarr/Recyclarr optional) runs on mnemosyne (DS923+, Synology
Container Manager) as a second compose project alongside Jellyfin — not on argus and
not on athena. It must be always-on (it polls indexers and monitors RSS continuously)
and it must share a filesystem with the library, because Sonarr/Radarr import by
hardlink or atomic move — which only works when the download directory and the media
library sit on one volume exposed under a single mount (`/volume1/Media Library` →
`/data` in every container); otherwise every import degrades to a slow full copy. argus
is disqualified — a 1GB Pi on an SD card, deliberately minimal and security-sensitive
(it isn't even a Syncthing peer, per #13), so constant SQLite churn plus download I/O
would both shred the card and bloat the one node we keep lean. athena is disqualified
because it is asleep by default (the entire premise of the WoL design, #18/ADR-0007);
a 24/7 acquisition stack there would either defeat that sleep posture or stall whenever
the desktop is down. This preserves the split ADR-0007 already draws: mnemosyne is the
always-on acquisition-and-serving tier, athena is on-demand transcode muscle only — the
Arr suite has nothing to do with the WoL path. The download side is Usenet-only for now
(SABnzbd + an indexer via Prowlarr): no seeding/ratio obligations, fast, SSL, and — the
deciding property for a NAS — no VPN required, so no Gluetun/killswitch container and no
torrent traffic from the home IP (torrents, if ever wanted, are an additive qBittorrent-
behind-Gluetun change, not a redo). Access is tailnet-only: every web UI is reached by
MagicDNS (`mnemosyne.<tailnet>.ts.net:<port>`) with nothing forwarded to the internet,
since all indexer/provider/download traffic is outbound from the NAS. The compose file
is not committed (NAS-side, carries provider/indexer API keys — matching the Jellyfin
and Syncthing precedent that the NAS lives outside the Nix repo); a sanitized template
and the build notes live in roadmap issue #20 instead. The trade-offs: the acquisition
stack shares the DS923+'s stock 4GB with Jellyfin, so the container set is kept lean and
SABnzbd's par2-repair spike is the memory ceiling to watch (RAM upgrade is the obvious
relief valve); and downloads live under `Media Library/` next to the served folders,
accepted because Jellyfin's libraries point at the specific `Shows/` and `Movies/`
subfolders, never the root, so it never scans the `downloads/` sibling.
