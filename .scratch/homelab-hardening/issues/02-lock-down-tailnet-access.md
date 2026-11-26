# 02: Lock down tailnet access

**What to build:** only the devices that need a service on the tailnet can
reach it, and the always-on nodes can't silently drop off the tailnet.

Today the tailnet runs Tailscale's default allow-all policy, and hephaestus — the
internet-facing DigitalOcean droplet — is a member. A compromise there could reach
DSM, WebDAV, the Jellyfin/Arr UIs, and every port on argus (whose firewall trusts
the whole `tailscale0` interface). Separately, Tailscale node keys expire after 180
days by default; argus was rebuilt around July 2026 and is headless, so it would
fall off the tailnet around January 2027 with no one at a console.

- Disable key expiry on argus and mnemosyne (the headless, always-on nodes).
- Replace the default policy with grants: tag hephaestus and give it no inbound
  access to home nodes (or only what it demonstrably needs); keep personal devices'
  current access.
- Narrow argus's tailnet firewall to the ports it actually serves (DNS, AdGuard UI,
  ssh) instead of trusting the whole interface.
- Record where the policy lives and why. If the policy file is committed, check it
  against AGENTS.md's public-repo rule first (it may contain account emails).

**Blocked by:** None (can start immediately).

**Status:** ready-for-human

- [ ] Admin console shows key expiry disabled for argus and mnemosyne; the reason is written down in the repo
- [ ] From hephaestus, connections to DSM, WebDAV, Jellyfin, and argus fail
- [ ] From hestia/athena/theseus, ssh, Syncthing, WebDAV, Jellyfin, and AdGuard all still work
- [ ] On argus, a tailnet port outside DNS/UI/ssh is closed; DNS + ssh over the tailnet still work
- [ ] Policy location + rationale documented (ADR if it's a fleet-wide posture change)
