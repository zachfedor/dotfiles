# AdGuard Home instead of Pi-hole on argus

argus (the always-on Pi, migrating Raspbian → NixOS) provides network-wide DNS
ad-block via AdGuard Home (`services.adguardhome`), replacing the Pi-hole it ran
under Raspbian. Pi-hole has no first-class NixOS module, so running it declaratively
means an OCI container (a Docker dependency on a small Pi, with config split between
Nix and the container) or a hand-rolled dnsmasq + web UI. AdGuard Home is a
like-for-like replacement — network-wide blocking, per-client rules, a web UI, and
built-in DoH/DoT upstream — with a native module whose `settings` live as plain Nix.
That fits the repo's top values, reproducibility and legibility (CONTEXT.md), which a
container-wrapped Pi-hole serves poorly. The trade-off is a one-time re-entry of
custom blocklists/allowlists into a new UI and leaving the familiar Pi-hole; accepted
as minor against a declarative config that matches how every other host is managed.
