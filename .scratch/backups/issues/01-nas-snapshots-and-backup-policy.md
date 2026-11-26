# 01: NAS snapshots and backup-policy ADR

**What to build:** a deleted, corrupted, or ransomware-encrypted file on the NAS
can be rolled back, and there is a written policy saying which data is protected
and how.

Today nothing is backed up. Syncthing is replication: deletes and corruption
propagate to every peer, NAS included. The NAS's RAID survives a dead disk, not a
bad sync, a mistaken delete, or ransomware. mnemosyne holds the only copy of
Shows, Movies, Music, Photos, and the container configs (Jellyfin metadata, Arr
databases, Syncthing keys).

- Check that the data volume is Btrfs. Snapshots depend on it, so if it isn't,
  stop and re-plan.
- Write an ADR that sorts data into **irreplaceable** (the PARA tree, photos,
  books, container configs, likely music) and **re-acquirable** (Shows/Movies via
  the Arr suite, downloads). Record the protection each class gets: snapshots,
  offsite (02), or neither. The ADR also corrects the "Syncthing backup" framing.
- Schedule snapshots with retention for the irreplaceable shares, with DSM's
  immutable-snapshot protection on, and the downloads folder excluded.

**Blocked by:** None (can start immediately).

**Status:** ready-for-human

- [ ] Data volume confirmed Btrfs (or the ticket is re-planned)
- [ ] ADR classifies every NAS share and the desktop-only data, with a protection level for each
- [ ] Scheduled, immutable snapshots exist for every share the ADR calls irreplaceable
- [ ] A deleted file was restored from a snapshot
- [ ] Snapshot retention and space use checked after the first week
