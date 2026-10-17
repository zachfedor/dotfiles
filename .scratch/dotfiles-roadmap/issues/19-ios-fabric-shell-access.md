# iOS → fabric shell access: an SSH app on theseus/daedalus over the tailnet

Status: needs-triage (proposed 2026-08-04)

## Origin

Spun out of **#18 (Wake-on-LAN)**. While planning the WoL phone-trigger, it became
clear that "tap something on the iPhone to run a command on a node" is a general
capability the fabric lacks, and WoL (`ssh argus wake-athena`) is only its
*thinnest* consumer. Rather than bury a real capability under the WoL issue, it
gets its own line.

Today there is **no interactive shell access to any node from the phone.** theseus
(iPhone) and daedalus (iPad) are on the tailnet only for sync (WebDAV to
mnemosyne); neither can open a shell on argus/athena/mnemosyne when something needs
a look away from the desk.

## Scope

An iOS SSH client, reaching nodes by MagicDNS name over the existing tailnet. Value
well beyond WoL:

- tail/adjust **AdGuard** or check **NUT/UPS** status on argus,
- kick a `nixos-rebuild` / read `journalctl` / restart a stuck `systemctl` unit on
  athena or mnemosyne,
- the general "I'm away from the desk and X needs a poke" case,
- and, incidentally, the #18 WoL trigger (`wake-athena`) — same key, same path.

## Sketch (not yet decided)

- **App choice.** Leading candidates:
  - **Blink Shell** — mosh support (survives the phone sleeping / network flaps,
    the actual pain of ssh-from-mobile), strong hardware-keyboard handling; paid.
  - **Termius** — cross-device host/key sync, friendlier UI; freemium.
  - (a-Shell / Prompt — lighter, less capable.) Pick one; don't collect.
- **Auth.** Generate the keypair **on the device**, add its pub key to each node's
  `authorizedKeys` in the repo (argus/athena already list Hestia+Athena keys) —
  fits the fabric's key-only, `PasswordAuthentication = false` posture. No key
  material in the flake, same rule as every other host key.
- **Reachability.** Tailscale (already on iOS) provides it; nodes resolve by
  MagicDNS name. Nothing new server-side beyond the authorized key.
- **mnemosyne.** Confirm it accepts an ssh key for a suitable user (Synology DSM
  ssh is opt-in and its own config surface, unlike the NixOS nodes).

## Open questions

- Which app (and is a paid tier worth it for mosh)?
- Whether daedalus (iPad) gets the same treatment or theseus only.
- Does this want any per-node hardening (a restricted command / forced-command key
  just for the WoL button vs. a full-shell key)? Probably overkill, but note it.

## Related

- **#18 Wake-on-LAN** — the parent; its Decision path B is this issue. The
  `ssh argus wake-athena` trigger is one consumer of this capability.
- **#13 Home server & media** — established the tailnet + argus/mnemosyne nodes.
- `docs/new-host.md` §6 — Tailscale `up` + per-app sign-in boundary this extends.
