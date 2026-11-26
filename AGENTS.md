## Public repository

This repo is public on GitHub (`zachfedor/dotfiles`). Everything committed,
including `.scratch/` issues and `docs/`, is world-readable and stays in git
history even after deletion.

- **No secrets.** Passwords, API keys, tokens, private keys, Tailscale auth keys,
  password hashes, Wi-Fi PSKs, and Usenet provider/indexer credentials never
  enter the repo. Point to an out-of-repo file instead (e.g.
  `/etc/nut/upsmon-password`) or keep them NAS-side, as the existing configs do.
- **No personal info.** No backup codes, account or ID numbers, addresses, phone
  numbers, or contents of documents from `~/areas` / `~/projects`.
- **Already public, by choice:** SSH public keys, hostnames, LAN IPs, Syncthing
  device IDs, athena's MAC, the tailnet name. Ask before adding more of this kind.
- When unsure, ask first and use a placeholder (`<tailnet>`, `CHANGE_ME`) in drafts.

## Agent skills

### Issue tracker

Issues live as markdown files under `.scratch/<feature>/` in this repo. See `docs/agents/issue-tracker.md`.

### Triage labels

Default triage vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: one `CONTEXT.md` + `docs/adr/` at repo root. See `docs/agents/domain.md`.
