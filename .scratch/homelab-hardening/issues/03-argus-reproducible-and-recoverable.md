# 03: Make argus reproducible and recoverable

**What to build:** if argus's SD card dies, a spare card flashed from the flake
brings network-wide DNS ad-block back to the same state, with `tailscale up` as
the only manual step — and the card doesn't slowly fill up in the meantime.

Today AdGuard Home's blocklists, allowlist, and settings exist only in its state
directory on the SD card (mutable settings, no declarative `settings`), so the
flake can rebuild the OS but not the service's actual behaviour.
`pihole-migration.md` already lists the live blocklists and the 11 allowlisted
domains to carry over. Nothing garbage-collects old Nix generations on argus or
athena, and there is no written cadence for pulling security updates.

- Declare AdGuard's filters and allowlist (as user rules) in Nix, keeping mutable
  settings so UI tweaks still work. Admin credentials stay out of the repo
  (AGENTS.md public-repo rule).
- Enable automatic GC and cap boot generations on argus and athena.
- Write down an update cadence (flake update + rebuild per host).
- Do the drill: flash a spare SD card from the flake, swap it in, and write down
  the recovery steps.

**Blocked by:** None (can start immediately).

**Status:** ready-for-human

- [ ] After a rebuild, AdGuard's UI shows the migrated blocklists + allowlist with no manual entry
- [ ] No admin password or hash is committed
- [ ] GC timer active and generation count capped on argus and athena
- [ ] Update cadence documented
- [ ] Spare SD card flashed from the flake restores DNS ad-block with only `tailscale up` by hand; steps written down
