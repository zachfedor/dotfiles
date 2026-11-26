# 02: Offsite backup

**What to build:** irreplaceable data survives losing the whole NAS — fire,
theft, a dead PSU taking out disks — because a copy lives somewhere else and has
been restored from at least once.

Snapshots (01) live on the same box. This ticket adds the off-box leg for whatever
the backup-policy ADR marks irreplaceable. Synology C2, Backblaze B2, and a
friend's NAS are all candidates. Pick on cost per TB and restore experience, and
record the choice in the ADR.

- Set up an encrypted, versioned Hyper Backup task for the irreplaceable shares
  to the chosen target.
- Keep the encryption key and target credentials in 1Password, never in the repo.
- Turn on DSM alerts for failed backup runs.
- Do a real restore test onto another machine, then write down the restore steps.

**Blocked by:** 01 (NAS snapshots and backup-policy ADR) — the ADR defines what
goes offsite.

**Status:** ready-for-human

- [ ] Offsite target chosen and recorded in the backup-policy ADR
- [ ] Encrypted, versioned backup task runs on a schedule for every irreplaceable share
- [ ] Encryption key + credentials stored in 1Password; none in the repo
- [ ] A failed run produces an alert
- [ ] A sample folder restored onto another machine and spot-checked
- [ ] Restore steps documented
