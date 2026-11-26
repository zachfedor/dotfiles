# 04: Plan for DNS and power failures

**What to build:** when argus or mains power goes down, the house and the tailnet
behave the way you decided ahead of time instead of the way they happen to.

**DNS.** argus is the LAN's DNS server on a Pi 3B+ SD card. Two facts aren't
recorded anywhere: what DNS servers the router hands out over DHCP, and whether
argus is set as the tailnet's global nameserver (possibly overriding local DNS).
If it's argus-only on both, a dead argus means "the internet is down" at home and
broken DNS on the phone away from home. Options: a second resolver (e.g. AdGuard
on the NAS, kept in sync) or accepting the risk on purpose, made tolerable by
ticket 03's quick recovery.

**Power.** The CyberPower UPS feeds the router, modem, and argus, with NUT on argus
shutting it down on low battery. The NAS — spinning disks, constant writes, the
only copy of most data — doesn't appear to be on it. If it isn't, put it on the UPS
and make DSM a NUT client of argus. DSM's client mode has fixed expectations: UPS
name `ups`, user `monuser`, password `secret`. So argus switches to netserver mode,
listens on the LAN, and allows the NUT port from the NAS only.

**Blocked by:** 03 (Make argus reproducible and recoverable) — "accept the risk"
is only a sensible option once recovery is quick.

**Status:** ready-for-human

- [ ] Router DHCP DNS servers and the tailnet nameserver setting are recorded
- [ ] ADR records the chosen DNS failure posture and why
- [ ] With argus powered off, LAN and tailnet DNS behave as the ADR says (tested)
- [ ] Whether mnemosyne is on a UPS is confirmed; if it wasn't, it now is
- [ ] DSM shows the UPS status from argus; a simulated low battery shuts the NAS down cleanly
- [ ] argus still shuts itself down on low battery
