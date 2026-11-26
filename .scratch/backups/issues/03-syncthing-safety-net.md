# 03: Syncthing safety net

**What to build:** an accidental delete or overwrite on one desktop can be undone
from another peer, and editor/OS junk files stop syncing and creating conflicts.

Today none of the seven Syncthing folders (gtd, projects, areas, resources,
archive, books, games) has file versioning, so a delete on hestia is gone from
athena and mnemosyne within seconds. There are no ignore patterns either, so macOS
`.DS_Store` files and Emacs lock, auto-save, and backup files all sync. Emacs lock
files are symlinks to targets that don't exist, which causes churn and conflicts.

- Turn on staggered versioning for every folder: in the declarative config for
  the desktops, and in the NAS's Syncthing GUI, since the NAS isn't managed by Nix.
- Add shared ignore patterns for every folder. Note that the ignore file itself
  doesn't sync between peers.
- Turn off Emacs lock files.

**Blocked by:** None (can start immediately).

**Status:** ready-for-human

- [ ] Every folder on hestia, athena, and mnemosyne has staggered versioning on
- [ ] A file deleted on hestia is recoverable from mnemosyne's version store
- [ ] A `.DS_Store`, Emacs lock file, or `*~` created on hestia never appears on athena
- [ ] Emacs no longer creates lock files
