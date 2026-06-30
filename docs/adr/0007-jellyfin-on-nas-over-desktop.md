# Jellyfin on the NAS, not the gaming desktop

The Jellyfin media server runs on mnemosyne (DS923+, Synology Container Manager),
co-located with the canonical media library, rather than on athena — the more
powerful machine (AMD CPU + GPU). The obvious objection is that the DS923+ (Ryzen
R1600, no iGPU) has *no* hardware transcoding and only weak 2-core software
transcoding, while athena could hardware-transcode easily. We chose the NAS anyway
because: (1) Jellyfin *direct-plays* when the client supports the codec — then the
server only ships bytes, which the R1600 does trivially — and the actual library
is SD DVD rips, the lightest possible load, with no streaming habits yet to justify
building for transcode load we can't predict; (2) the NAS never sleeps, so the
library and server stay co-located with no "desktop must be awake" dependency, no
network hop, and far lower power per multi-hour stream; (3) hosting a 24/7 service
on athena would couple it to a machine we want free to reboot, suspend, update, and
game on. The trade-off is no GPU transcoding, so heavy 4K/HEVC to browsers or remote
cellular clients would choke. The escape hatch (documented, not built): move
Jellyfin to athena and wake it on demand via Wake-on-LAN, using argus (the always-on
Pi, ethernet, same LAN) as the magic-packet relay per Tailscale's WoL pattern. That
is an additive change if the need ever materialises, not a redo — so we start simple.
