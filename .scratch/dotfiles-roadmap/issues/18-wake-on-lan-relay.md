# Wake-on-LAN: wake athena on demand, argus as the always-on LAN relay

Status: needs-triage (proposed 2026-08-04)

## Origin

Both **ADR-0007** (Jellyfin on the NAS, not the desktop) and **CONTEXT.md** name
Wake-on-LAN as a *documented-but-not-built* escape hatch, yet no roadmap issue
existed for it. This is that issue.

The load-bearing sentence, from ADR-0007:

> The escape hatch (documented, not built): move Jellyfin to athena and wake it on
> demand via Wake-on-LAN, using argus (the always-on Pi, ethernet, same LAN) as the
> magic-packet relay per Tailscale's WoL pattern. That is an additive change if the
> need ever materialises, not a redo — so we start simple.

The trigger is the DS923+ transcode ceiling (Ryzen R1600, no iGPU). While the
library is SD DVD rips that direct-play, the NAS is fine. If 4K/HEVC to a browser
or a remote cellular client ever forces software transcode, the answer is athena's
RX 5700 XT — but athena is the machine we deliberately keep free to reboot,
suspend, update, and game on (ADR-0007), so it must **sleep by default and wake on
demand**. WoL is the primitive that makes "asleep by default" compatible with
"serves media when asked."

## Scope: a primitive, not just the Jellyfin hatch

WoL is worth building as a small reusable capability, because the same
"argus wakes athena" path serves three uses — only the first is from ADR-0007:

1. **Jellyfin transcode escape hatch** (the ADR-0007 motivation). Needs the most
   automation (wake *before* a client stalls) — deferred to a later slice.
2. **Remote nix build box.** `495ffa9 feat(argus): allow remote nix rebuilds`
   already lets athena be driven remotely; letting a build wake a sleeping athena
   makes it a true on-demand builder for hestia/argus instead of an always-on one.
3. **Local LLM agent (issue 17).** athena hosts Ollama-Vulkan on `:11434`. Waking
   it from hestia to run the Pi/Gondolin agent, then letting it sleep, fits the
   same on-demand posture.

So v1 delivers the **manual wake path** (the primitive + firmware + NixOS config);
the Jellyfin **wake-on-demand automation** is a separate, later slice.

## Topology (verified on athena, 2026-08-04)

```
  argus (192.168.1.2, eth0)  ──LAN 192.168.1.0/24──  athena (192.168.1.3, enp4s0)
  always-on Pi, Tailscale                            asleep by default
  sends magic packet  ───────────► MAC 24:4b:fe:00:1d:5a  wakes (S3/S5)
```

- **Target:** athena, NIC `enp4s0`, MAC `24:4b:fe:00:1d:5a`, driver **r8169**
  (Realtek Gigabit — the magic-packet `wol g` mode is supported on essentially all
  r8169 cards; confirm with `ethtool enp4s0` → `Supports Wake-on: ... g`). Wired,
  same `/24` as the relay.
- **Relay:** argus, `eth0` at `192.168.1.2`, always-on, on the same physical LAN
  and broadcast domain — the one prerequisite WoL needs (magic packets are
  layer-2, don't route across subnets). Argus already satisfies it.

## Decision: argus runs the WoL sender; Tailscale is only reachability

**Correction from Tailscale's own WoL write-up** ([blog: Wake-on-LAN + UpSnap](
https://tailscale.com/blog/wake-on-lan-tailscale-upsnap)): Tailscale **cannot send
the magic packet itself** — *"Tailscale operates on the network layer (Layer 3)...
Tailscale can't send WoL packets."* Magic packets are layer-2, so there is **no**
admin-console "wake this node" button that relays for you. "Per Tailscale's WoL
pattern" (the ADR-0007 phrase) therefore means exactly one thing: **Tailscale gets
you to an always-on LAN relay, and that relay emits the packet.** The relay is
argus; the only real choice is *what runs on argus* and *how you trigger it*.

### The sender on argus — `wakeonlan` / `etherwake` (CLI)  ✅ ship
`pkgs.wakeonlan` (or `etherwake`, the exact tool the blog uses:
`sudo etherwake AA:11:BB:22:CC:33`) on argus + a tiny `wake-athena` wrapper baked
into `hosts/argus/default.nix`; trigger with `ssh argus wake-athena` (over LAN or
tailnet ssh — Tailscale's job is just making argus reachable from off-LAN).
- Fully declarative on argus, trivially scriptable — this is the primitive the
  build-box and LLM-agent uses (and any eventual Jellyfin automation) hang off of.
  One line to invoke.
- You must be able to reach argus, which is always true in practice (LAN or tailnet).

### The trigger from iOS (theseus/daedalus)

The CLI wrapper is trivial from a desktop, but the primary manual case is **tap
something on the iPhone**, and there's no ssh client set up on iOS today. All the
options below still call the *same* argus-side `wake-athena` — they differ only in
the front-end. Three, lightest-on-argus first:

**A — iOS Shortcuts → "Run Script Over SSH"  ✅ recommended first**
A built-in Shortcuts action (Scripting) that ssh's to argus over the tailnet and
runs `wake-athena`, surfaced as a home-screen icon / widget / "Hey Siri, wake
athena". **Nothing new on argus.** Auth fits argus's existing posture cleanly: set
the action's Authentication to *SSH Key* → Shortcuts **generates the keypair
in-app** → *Share Public Key* → append it to `~/.ssh/authorized_keys` on argus.
That's key-only, no password — exactly argus's `PasswordAuthentication = false`
rule. In the repo this is one more entry in argus's `authorizedKeys.keys`, same as
hestia/athena. ([setup detail][ios-ssh-shortcut])

**B — A real iOS SSH app (Blink / Termius / Prompt)  ✅ worth it for its own sake**
Over Tailscale, an iOS terminal reaches argus by MagicDNS name and runs
`wake-athena` — but WoL is the *thinnest* reason to want this. The fabric currently
has **no interactive shell access to any node from the phone**, and an SSH app
closes that gap generally: tailing AdGuard on argus, kicking a `nixos-rebuild` or
checking `systemctl`/`journalctl` on athena or mnemosyne, poking the NUT/UPS status
(argus issue), restarting a stuck service — all the "I'm away from the desk and
something needs a look" cases. Blink adds **mosh** (survives the phone sleeping /
network flaps, the actual pain of ssh-from-mobile) and hardware-keyboard support;
Termius syncs keys/hosts across devices. This is really a **separate small
capability** — "iOS → fabric shell access" — that WoL merely happens to be the
first consumer of; it deserves its own line on the roadmap rather than being
smuggled in under WoL. Key setup mirrors A (generate on device, add the pubkey to
each node's `authorizedKeys`).

**C — UpSnap web UI  ⚠️ only if A/B don't satisfy**
The blog's headline tool: a self-hosted webapp giving *"a browser-based dashboard
for sending Wake-on-LAN packets,"* reachable by MagicDNS hostname — genuinely the
nicest *browser* button, no ssh at all. The cost is on argus: UpSnap (PocketBase —
a Go binary + embedded SQLite, so its writes are small; the SD-wear worry was
overstated) still means **a persistent networked service on the deliberately-minimal
always-on node**, and there's **no NixOS module for it**, so it's either enabling
the Docker stack on the Pi (`network_mode: host` for LAN access) or a hand-rolled
systemd unit around the standalone binary. That's a real legibility/maintenance
cost on the security-sensitive node — not "invalid," just the heaviest of the
three. Stand it up only if the Shortcuts button (A) proves clunky in practice
(on-device key handling can be fiddly) and a dashboard is wanted badly enough.

**Recommendation:** ship the `wakeonlan` CLI wrapper on argus (it *is* the
Tailscale WoL pattern and unblocks all three uses), and for the phone use **A
(iOS Shortcuts)** as the WoL button. Treat **B (SSH app)** as its own worthwhile
roadmap item for general fabric access — not gated on WoL. Hold **C (UpSnap)** as
the fallback only if A disappoints.

[ios-ssh-shortcut]: https://matsbauer.medium.com/how-to-run-ssh-terminal-commands-from-iphone-using-apple-shortcuts-ssh-29e868dccf22

## Setup / implementation sketch

Declarative wherever the repo can own it; the firmware toggle is the one
irreducible imperative step.

1. **athena — enable WoL on the NIC (declarative, through NetworkManager).**
   ⚠️ The obvious `networking.interfaces.enp4s0.wakeOnLan.enable = true` **does not
   work** on athena and was tried first — see friction #1 (RESOLVED). Because NM
   owns the link, that option generates no systemd unit and NM resets WoL to the
   driver default (disabled). Drive it through NM instead, in
   `hosts/athena/default.nix`:
   ```nix
   networking.networkmanager.connectionConfig."ethernet.wake-on-lan" = 64;
   # 64 = 0x40 = NM_SETTING_WIRED_WAKE_ON_LAN_MAGIC
   ```
   This sets the global per-connection default so NM itself puts the NIC in
   magic-packet mode on activation. Verify after switch: `sudo ethtool enp4s0` →
   `Wake-on: g`, and re-check after an NM connection cycle / reboot.

2. **athena — firmware (imperative, document in `docs/new-host.md`).** Enable the
   UEFI/BIOS WoL option (Realtek boards: *"Power On By PCI-E / PCIe Devices"*,
   sometimes *"Resume By PCI-E"*; also **disable ErP / EuP** deep-off, which cuts
   NIC standby power and kills WoL from S5). Without this the OS-level setting is
   inert. This is exactly the kind of undocumented manual step CONTEXT.md value #1
   exists to capture — so it goes in `docs/new-host.md`, not tribal memory.

3. **argus — the relay/script (declarative, option B).** In
   `hosts/argus/default.nix`, add `wakeonlan` and a wrapper:
   ```nix
   environment.systemPackages = with pkgs; [ git neovim wget wakeonlan ];
   # convenience: `ssh argus wake-athena`
   environment.shellAliases.wake-athena =
     "wakeonlan 24:4b:fe:00:1d:5a";
   ```
   (Or a `pkgs.writeShellScriptBin "wake-athena"` if it should be a real binary on
   PATH for non-interactive ssh — an alias won't fire under `ssh argus wake-athena`
   unless the login shell sources it, so prefer the script form. Decide during
   build.) MAC is stable/hardware-bound, so hardcoding it is fine; a comment should
   cross-reference athena's `enp4s0`.

4. **Off-LAN / phone trigger.** From a desktop the trigger is just
   `ssh argus wake-athena` over the tailnet — nothing new. For the iPhone, build an
   **iOS Shortcut** ("Run Script Over SSH" → argus → `wake-athena`, SSH-key auth,
   pubkey added to argus's `authorizedKeys` — see Decision path A). (This is the
   whole of what "Tailscale's WoL pattern" buys: reachability to the relay, not a
   magic-packet button.)

5. **Verify (v1 done):** suspend athena (`systemctl suspend`), then from argus (or
   over tailnet ssh) run `wake-athena`; confirm it powers back and SSH returns.
   Also test from **S5** (full `poweroff`), which is the state that matters for the
   power-saving escape hatch — S5 WoL is what the ErP toggle in step 2 governs.

## Points of friction (unresolved)

1. **NetworkManager vs the ethtool WoL state — RESOLVED 2026-08-08.** Predicted
   risk confirmed the hard way: `networking.interfaces.enp4s0.wakeOnLan.enable`
   left `Wake-on: d` after a full rebuild. Root cause = two things, not one: (a)
   under NM that option generates *no* systemd unit at all (it targets
   scripted/networkd links), so nothing ever ran `ethtool -s ... wol g`; and (b) NM
   itself resets WoL to the driver default (disabled) on every (re)activation, its
   `802-3-ethernet.wake-on-lan` sitting at `default`. Fix = go through NM:
   `networking.networkmanager.connectionConfig."ethernet.wake-on-lan" = 64` (magic).
   See setup step 1. Still must confirm `Wake-on: g` post-switch + across an NM cycle.
2. **amdgpu suspend/resume reliability.** The escape hatch wants athena to
   auto-suspend when idle and resume cleanly on wake — but issue 17's spike log
   already documents this RX 5700 XT (gfx1010) being touchy under GPU load
   (compute hangs froze GNOME). Suspend/resume is a *different* code path than
   compute, but resume glitches on amdgpu are common enough to **test explicitly**
   before trusting unattended auto-suspend. If resume is flaky, the hatch may need
   S5 (full off) rather than S3, which is slower to wake but avoids the resume
   path entirely.
3. **Wake-on-demand automation is the actually-hard part (deferred).** Manual WoL
   (v1) doesn't help a Jellyfin client that connects to a *sleeping* athena — the
   stream stalls before anything sends the packet. True on-demand needs a shim:
   something always-on (argus or mnemosyne) that intercepts the connection, fires
   WoL, waits for athena + Jellyfin to come up, then proxies/redirects. That's a
   whole slice (reverse proxy + health-gate + timeout UX) and is **out of v1
   scope** — v1 just proves the wake primitive.
4. **Idle/sleep policy** (paired with #2/#3). For the hatch to save power athena
   must auto-suspend when idle *and stay awake while streaming* — a
   logind `IdleAction` / GNOME power policy plus a "held awake while Jellyfin has a
   session" mechanism. Design alongside the automation slice, not v1.
5. **Trigger for uses 2/3.** Waking athena as a build box or for the LLM agent is
   just `ssh argus wake-athena` before the job — but that wants a wrapper on
   *hestia* (e.g. a `wake-and-wait` that WoLs then blocks on athena's ssh coming
   up). Small, but decide whether it lives in `home.nix` (cross-machine) or a
   `scripts/` helper.

## Next-actions for exploration

- [x] **Confirm NIC WoL support** — DONE 2026-08-04, `nix-shell -p ethtool` +
      `sudo ethtool enp4s0` on athena confirms magic-packet WoL supported (r8169).
- [x] ~~`networking.interfaces.enp4s0.wakeOnLan.enable`~~ — no-op under NM
      (friction #1), replaced by the NM route below.
- [ ] **athena config:** `networking.networkmanager.connectionConfig."ethernet.wake-on-lan" = 64`
      in `hosts/athena/default.nix` (done in-repo, needs deploy); `rebuild`; verify
      `sudo ethtool enp4s0` → `Wake-on: g` sticks across a reboot and an NM cycle.
- [ ] **Firmware:** enable Power-On-By-PCIe + disable ErP in athena's UEFI;
      record the exact menu path in `docs/new-host.md`.
- [ ] **argus config:** add `wakeonlan` + a `wake-athena` script (not just an
      alias — friction/step 3) to `hosts/argus/default.nix`; rebuild argus.
- [ ] **iOS Shortcut (WoL button):** "Run Script Over SSH" → argus → `wake-athena`;
      generate the key in Shortcuts, add its pubkey to argus's `authorizedKeys`
      (Decision path A). Surface as a home-screen/widget/Siri button.
- [ ] **Verify v1:** wake athena from both S3 (suspend) and S5 (poweroff) via
      `ssh argus wake-athena`, on LAN, over tailnet ssh, and via the iOS Shortcut.
- [ ] **Spike amdgpu resume** (friction #2) before committing to auto-suspend.
- [ ] **Spin a separate roadmap item: "iOS → fabric shell access"** (Decision path
      B) — an iOS SSH app (Blink/mosh or Termius) with pubkeys on argus/athena/
      mnemosyne, for general remote ops from the phone. WoL is only its first
      consumer; don't bury it under this issue.
- [ ] **(Later slice, not v1)** design Jellyfin wake-on-demand: intercepting relay
      + health-gate + idle/keep-awake policy (frictions #3/#4).
- [ ] If v1 ships and the hatch is ever exercised, the "Jellyfin moved to athena"
      decision would warrant its own ADR superseding/extending ADR-0007.

## Related

- **ADR-0007** (Jellyfin on the NAS over the desktop) — the source of the escape
  hatch this issue implements; the "additive, not a redo" promise made here.
- **CONTEXT.md** — argus described as "the on-LAN Wake-on-LAN relay for the
  Jellyfin escape hatch"; Streaming-server escape-hatch note.
- **#13 Home server & media** (done) — stood up argus (Tailscale + AdGuard),
  the always-on relay this depends on; also the Jellyfin-on-mnemosyne decision.
- **#17 Local LLM coding agent** — athena's Ollama-Vulkan on `:11434`; a
  secondary consumer of the wake primitive (use #3) and the source of the
  amdgpu-reliability caution in friction #2.
- `495ffa9 feat(argus): allow remote nix rebuilds` — enables use #2 (on-demand
  build box) once WoL lets a build wake a sleeping athena.
- **Tailscale blog — [Wake-on-LAN with Tailscale + UpSnap](
  https://tailscale.com/blog/wake-on-lan-tailscale-upsnap)** — the authoritative
  statement that Tailscale is L3 and can't send magic packets (so an on-LAN relay
  running `etherwake`/`wakeonlan` is mandatory), plus UpSnap as the optional web-UI
  front-end weighed above.
