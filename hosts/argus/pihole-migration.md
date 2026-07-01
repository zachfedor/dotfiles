# Pi-hole → AdGuard Home migration reference (argus, issue 13 slice 2)

Captured from the old Raspbian Pi-hole (`raspi`) before retiring it. Nothing here
is applied automatically — reconcile into AdGuard Home's config when setting it up
(web UI first-run, or later declaratively in `hosts/argus/default.nix`). Unsure how
much is still needed; kept for reference.

## Old node facts
- DNS-only (no DHCP server, no static leases, no local DNS records, no CNAMEs)
- Interface eth0, 192.168.1.2/24; cache-size 10000; DNSSEC off
- Extra services dropped intentionally: PiVPN/WireGuard (Tailscale replaces it),
  lighttpd admin panel (AdGuard has its own UI), exim4 (cron mail)

## Upstream resolvers (old set, plain :53)
Cloudflare 1.1.1.1 / 1.0.0.1
Google 8.8.8.8 / 8.8.4.4
Quad9 2620:119:35::35 / 2620:119:53::53
OpenDNS 208.67.222.222 / 208.67.220.220
(AdGuard's defaults already cover these.)

## Allowlist (11 domains, Pi-hole whitelist type=0)
angelfire.com
api.rollbar.com
cdn.cookielaw.org
collector.githubapp.com
fast.fonts.net
fonts.net
lcprd1.samsungcloudsolution.net
metrics.brightcove.com
openload.co
spade.twitch.tv
vortex.data.microsoft.com

## Blocklists — LIVE (worth re-adding on top of AdGuard defaults)
https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts
https://adaway.org/hosts.txt
https://v.firebog.net/hosts/static/w3kbl.txt
https://v.firebog.net/hosts/AdguardDNS.txt
https://v.firebog.net/hosts/Easyprivacy.txt
https://v.firebog.net/hosts/Prigent-Ads.txt
https://s3.amazonaws.com/lists.disconnect.me/simple_tracking.txt
https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt
https://s3.amazonaws.com/lists.disconnect.me/simple_malvertising.txt

## Blocklists — DEAD (shut down years ago; do NOT port)
https://mirror1.malwaredomains.com/files/justdomains        # malwaredomains, gone 2019
https://zeustracker.abuse.ch/blocklist.php?download=...      # abuse.ch retired zeustracker
http://sysctl.org/cameleon/hosts                            # dead
https://hosts-file.net/ad_servers.txt                       # hpHosts shut down 2019/2020
https://hosts-file.net/grm.txt
https://hosts-file.net/exp.txt
https://hosts-file.net/emd.txt
https://hosts-file.net/psh.txt
https://reddestdream.github.io/.../minimalhosts             # stale
# also several StevenBlack sub-lists (KADhosts/add.Spam) — already covered by the
# main StevenBlack/hosts consolidated list above.
