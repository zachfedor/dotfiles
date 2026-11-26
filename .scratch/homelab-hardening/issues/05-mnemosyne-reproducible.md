# 05: Make mnemosyne reproducible

**What to build:** the NAS's services can be rebuilt from this repo and updated
safely, instead of depending on compose files and setup steps that exist only on
the NAS and in old issue notes.

Today the Jellyfin, Syncthing, and (soon) Arr compose projects live only on the
NAS, deliberately kept out of the repo because some carry credentials. The setup
steps are spread across #06, #13, and #20: the Tailscale package, the WebDAV
server, Container Manager projects, and the uid-1026 ACL gotcha. Every image uses
`:latest`, and Container Manager doesn't update on its own, so images either go
stale or break whenever someone happens to pull.

- Commit sanitized compose files with all secrets in an uncommitted env file
  (AGENTS.md public-repo rule).
- Write a single NAS rebuild runbook that consolidates the scattered steps.
- Pin images to major versions and document an update routine: snapshot, pull,
  check.
- Optional: valid HTTPS for WebDAV on mnemosyne's MagicDNS name (Tailscale
  certificate), replacing the raw Tailscale IP the phone uses today.

**Blocked by:** backups/01 (NAS snapshots and backup-policy ADR) — the update
routine starts with a snapshot.

**Status:** ready-for-human

- [ ] Compose files for every NAS project are committed; a secret scan of the diff finds nothing
- [ ] NAS rebuild runbook covers Tailscale, WebDAV, each Container Manager project, and the ACL fix
- [ ] No image uses `:latest`
- [ ] One real image update done by the documented routine (snapshot taken first)
- [ ] (Optional) iOS Files connects to WebDAV by MagicDNS name with no certificate warning
