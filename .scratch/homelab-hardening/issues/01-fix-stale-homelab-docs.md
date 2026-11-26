# 01: Fix stale homelab docs

**What to build:** the homelab docs match what is actually deployed, so a future
reader (or a fresh-host rebuild) isn't misled by instructions that were disproved
or superseded during #13/#18.

- The new-host guide's Wake-on-LAN step still recommends the per-interface
  `wakeOnLan` option, which #18 proved is a no-op under NetworkManager; point it at
  the NetworkManager connection default athena actually uses.
- The new-host guide links ADR-0002 by a filename that doesn't exist.
- CONTEXT.md's media table calls the NAS's Syncthing copy of games a "backup
  replica" (sync propagates deletes; it isn't a backup) and says games reach
  "athena only", though #13 made them all-3-peer.
- CONTEXT.md doesn't say Calibre's library DB is synced, so only one machine may
  have Calibre open at a time.
- argus's Tailscale comment blames the old `tailscale ssh` failure on stale host
  keys; #13 found the real cause (argus doesn't advertise Tailscale SSH).
- #18's status and checklist don't reflect that `wake-athena` and the NM WoL
  setting are already in the config.
- The org-sync script targets a `~/org` directory that no longer exists (and
  git-committing inside a Syncthing folder is a bad mix) — remove it.

**Blocked by:** None (can start immediately).

**Status:** ready-for-agent

- [ ] New-host WoL step matches athena's NetworkManager-based config
- [ ] Every relative link in the new-host guide resolves to a real file
- [ ] CONTEXT.md media table: games reach all three peers; no "backup" wording for Syncthing copies
- [ ] CONTEXT.md records the one-Calibre-at-a-time rule
- [ ] argus Tailscale comment states the real `tailscale ssh` root cause
- [ ] #18 status + checklist reflect what's already built (deploy/verify items stay open)
- [ ] Dead org-sync script removed, with no remaining references
