# Local & hybrid LLM coding agent: Pi + Gondolin micro-VM sandbox

Status: needs-triage (proposed 2026-07-28)

## Origin

Sparked by wanting to try local LLMs through the **Pi** coding agent (pi.dev) on
athena, and reviewing Pi's three containerization patterns
(https://pi.dev/docs/latest/containerization). Two constraints drove the choice:

1. **athena's hardware** — Ryzen 7 3700X (8c/16t), **16 GB RAM**, **Radeon RX 5700
   XT (Navi 10, gfx1010, 8 GB VRAM)**, `amdgpu`. No CUDA, so inference goes through
   Vulkan or (finicky) ROCm, not the NVIDIA fast path.
2. **Trust framing (user's own words)** — "I don't trust their tool calls, nor
   their outputs." Not about weights or open-vs-closed. The stance is that *no*
   model — local **or** publicly "trusted" hosted ones (Claude, GPT) — has a model
   of truth or consequence, so any of them can emit a destructive tool call or a
   confident-but-wrong output.

## Threat model (the thing the design must satisfy)

Two axes that are easy to conflate but must be kept separate:

- **Behavioral trust (tool calls + outputs): uniformly zero, all models.** Failure
  mode isn't malice, it's incoherence. Mitigation splits in two:
  - *Tool-call blast radius* → **sandbox execution for every model,
    unconditionally** (not just local ones).
  - *Output correctness* → **human verification**; containment does nothing here.
    This axis stays on the user and is out of scope for the sandbox.
- **Credential & network needs: differ by model type.** A local model needs no
  secret and no egress. A hosted/subscription model needs a **valuable credential
  present** and an **outbound route**. Design goal: the execution sandbox can
  **never reach the real credential**, regardless of which model is driving —
  including the paid one.

## Decision: Gondolin (Pi's local micro-VM extension)

Pi ships three patterns; picking Gondolin over the other two:

### Gondolin — CHOSEN
A QEMU micro-VM that runs Pi's built-in tools (`read`, `write`, `edit`, `bash`,
`grep`, `find`, `ls`) and `!` commands inside the VM, mounting host cwd at
`/workspace` with changes synced back. **Auth stays on the host.**

- **Pros:**
  - Sandboxes tool execution for *every* model → satisfies "behavioral trust zero"
    uniformly.
  - "Auth remains host-based" → the provider credential (`~/.pi/agent/auth.json`
    for `/login` subscription tokens, or an API key) never enters the sandbox where
    incoherent tool calls run. Directly satisfies the credential axis, and it holds
    against the *paid* model too, not just local ones.
  - Inference runs on the host, so host Ollama gets full GPU access; the VM just
    reaches the endpoint over the network. Because the model lives in **VRAM**, the
    micro-VM's ~1–2 GB RAM overhead does **not** compete with the model — so it
    doesn't force a smaller/worse model on athena's tight 16 GB.
  - Purpose-built by the Pi authors for exactly this isolation scenario.
- **Cons:**
  - Two extra deps: **Node.js ≥ 23.6.0** and **QEMU system emulation**.
  - **Install is imperative** (`cp -R … ~/.pi/agent/extensions/gondolin` +
    `npm install`) — fights the repo's reproducibility value (see friction #3).
  - `/workspace` is a sync, not a live bind — occasionally surprising.

### Plain Docker — REJECTED
Whole Pi process in a container, `/workspace` bind-mounted.
- Simplest, and athena already has a native Docker daemon (ADR-0006). But it puts
  the credential in the *same* box as tool execution ("provider API keys enter the
  container" per Pi's docs). To keep it safe you'd bolt on a host-side inference
  proxy (e.g. LiteLLM) holding all real keys, with the container pointed at one
  localhost endpoint and egress locked to it — i.e. reimplementing what Gondolin
  and OpenShell give natively. More moving parts for a weaker boundary.

### OpenShell — REJECTED
NVIDIA's policy-controlled sandbox (local gateway or remote k8s) with
filesystem/process/network/credential/inference controls.
- Its inference-routing/credential-isolation is genuinely relevant once paid models
  are in play, but it's enterprise/k8s-shaped and NVIDIA-oriented — overkill on a
  single AMD workstation. Revisit only if this outgrows one box.

## Architecture

```
  ┌─────────────────────────── athena (host) ───────────────────────────┐
  │                                                                      │
  │  Pi agent process  ──── holds auth (~/.pi/agent/auth.json / API key) │
  │      │                                                               │
  │      ├── local model  ─────────────►  Ollama (host, GPU via amdgpu)  │
  │      │                                 model weights live in 8GB VRAM│
  │      ├── hosted/sub model ─────────►  api.anthropic.com (host egress)│
  │      │                                                               │
  │      └── tool calls / ! commands ──►  ┌───────── Gondolin VM ──────┐ │
  │                                       │  QEMU micro-VM             │ │
  │                                       │  /workspace ⇄ host cwd     │ │
  │                                       │  read/write/edit/bash/…    │ │
  │                                       │  NO credential reachable   │ │
  │                                       └────────────────────────────┘ │
  └──────────────────────────────────────────────────────────────────────┘
```

The container/VM boundary is orthogonal to model choice: one `models.json` holds
both an Ollama entry and a hosted entry; switch via `/model`. Local calls hit host
Ollama, hosted calls leave from the host with host-held auth, tool execution is
always sandboxed.

## Setup / implementation sketch

Declarative where the repo can own it; imperative only where Pi forces it.

1. **Node.js ≥ 23.6** — add `nodejs_24` to `home.nix` packages (cross-platform;
   CONTEXT.md already sanctions `nodejs` as a thin global fallback). Verify the
   26.05 nixpkgs `nodejs_24` is ≥ 23.6 (it is).
2. **QEMU system emulation** — add `qemu` to `hosts/athena/default.nix`
   `environment.systemPackages`. NOTE: athena already registers a qemu **binfmt**
   handler (`boot.binfmt.emulatedSystems = ["aarch64-linux"]`) for argus's SD
   image — that's `qemu-user`, *not* the `qemu-system-x86_64` a local VM needs.
   Confirm which binary Gondolin invokes and that it's on PATH.
3. **Host Ollama with GPU** — add `services.ollama` to `hosts/athena/default.nix`.
   Acceleration is the big open question (friction #1): ROCm with a gfx1010
   override vs Vulkan backend. Start conservative, measure.
4. **Gondolin extension** (imperative, from Pi's docs):
   ```bash
   cp -R packages/coding-agent/examples/extensions/gondolin ~/.pi/agent/extensions/gondolin
   cd ~/.pi/agent/extensions/gondolin && npm install --ignore-scripts
   ```
   Wrap this in `scripts/` + document in `docs/new-host.md` so a fresh box is
   reproducible (see friction #3).
5. **Pi model config** — `~/.pi/agent/models.json` with two entries:
   - local: `baseUrl: http://localhost:11434/v1`, `api: openai-completions`,
     `apiKey: "ollama"` (dummy).
   - hosted: Anthropic via `/login` (subscription token → `auth.json`) or API key.
6. **Run from a project:** `cd <project> && pi -e ~/.pi/agent/extensions/gondolin`.

## Points of friction (unresolved)

1. **GPU inference on gfx1010.** Navi 10 (RX 5700 XT) isn't on ROCm's officially
   supported list. Options: (a) ROCm with `HSA_OVERRIDE_GFX_VERSION` / nixpkgs
   `rocmOverrideGfx`; (b) Ollama's Vulkan backend; (c) CPU fallback (slow, and it
   *would* then compete with the VM for the 16 GB — the one case where Gondolin
   costs model quality). This single choice gates whether the whole plan performs.
   **Needs a spike before committing.**
2. **Does Gondolin actually keep the credential out of the VM, and can egress be
   restricted?** The docs assert "auth remains host-based" but are silent on the
   VM's network config and whether tool-side egress can be locked down. The entire
   credential-axis guarantee rests on this — **verify empirically**, don't assume.
3. **Imperative install vs reproducibility (CONTEXT.md value #1).** The `cp` +
   `npm install` extension setup is exactly the "undocumented manual step" the repo
   exists to avoid. Decide the reproducibility mechanism: a `scripts/` installer +
   `docs/new-host.md` entry (lightest), a home-manager activation script, or
   vendoring the extension. Don't let it become tribal knowledge.
4. **RAM headroom.** 16 GB with the VM + host desktop (GNOME) + a 7–8B model. Fine
   *if* the model is in VRAM (friction #1 resolves to GPU); tight-to-painful if it
   spills to CPU. Bounds model size.
5. **Which local models.** `llmfit` output was overwhelming. Cut the paralysis with
   a hard constraint: 8 GB VRAM ⇒ **7–8B dense coding model at Q4_K_M** (~5 GB,
   leaves room for context). Shortlist to benchmark, not agonize over:
   Qwen2.5-Coder-7B-Instruct, DeepSeek-Coder-6.7B. Skip 14B+ (spills VRAM) and MoE
   (16B weights don't fit) for now.

## Spike log

### 2026-07-30 — GPU inference, attempt 1 (ROCm + gfx override): FAILED, froze desktop

Ran on athena directly (no config commit — used `nix run`-style store builds of
nixpkgs `ollama-rocm` / `ollama-vulkan` / `ollama` at 0.30.6, pinned rev
`714a5f8`). Hardware confirmed: RX 5700 XT, PCI `1002:731F`, **gfx1010**
(`gfx_target_version 100100`), 8 GB VRAM, `/dev/kfd` + `/dev/dri/renderD128` are
`0666` (world-accessible, so no `render`-group membership needed for the test).

**Result:** `ollama-rocm` with `HSA_OVERRIDE_GFX_VERSION=10.3.0` (the standard
Navi 10 → gfx1030 trick) **hung the GPU and froze the GNOME session.** Kernel log
showed `GCVM_L2_PROTECTION_FAULT` / `PERMISSION_FAULTS: 0x3` → `ring gfx_0.0.0
timeout` → `device wedged, but recovered through reset`, with KFD compute queues
evicted. The kernel survived (single unbroken boot, `amdgpu` self-reset the ring);
what "crashed" was the desktop.

**Two distinct findings — keep them separate:**

1. **The gfx override itself is unsafe here, not just slow.** Forcing RDNA2
   (gfx1030) rocBLAS kernels onto RDNA1 (gfx1010) silicon produced *illegal memory
   accesses*, not a clean "unsupported" refusal. So option (a) in friction #1
   (ROCm + `HSA_OVERRIDE_GFX_VERSION`) is **rejected** for this card. (Untested
   alternative: a rocBLAS actually built with gfx1010 Tensile kernels — nixpkgs
   `rocmPackages` almost certainly doesn't ship those, so treat as out of reach.)
2. **Structural: one GPU is shared by compute and the display.** The fault landed
   on `gfx_0.0.0` — the same ring `gnome-shell-wr` renders on — so *any* GPU-compute
   hang freezes the desktop, regardless of backend. This raises the stakes on the
   remaining Vulkan attempt and is a real cost to weigh vs. CPU-only. New friction
   for the issue body: **inference-vs-desktop GPU contention / blast radius.**

**System state after:** fully recovered — no stray `ollama serve`, VRAM back to
~330 MB, GPU 0% busy, no new faults post-reset. `qwen2.5-coder:7b` (Q4, ~4.4 GB)
finished pulling and is cached in `~/.ollama`, so a retry won't re-download.

**Next GPU option = Vulkan** (`ollama-vulkan`, RADV — the same well-supported path
Steam/games use, so much less likely to fault than the RDNA2-on-RDNA1 hack). But
it still shares the GPU with GNOME per finding #2. Tested next — see below.

### 2026-07-31 — GPU inference, attempt 2 (Vulkan): SUCCESS — friction #1 resolved

`ollama-vulkan` (RADV NAVI10) on the same cached `qwen2.5-coder:7b` Q4. **Stable,
full GPU offload, no faults, desktop survived.**

| backend | processor | prefill | decode | full 256-tok reply |
|---|---|---|---|---|
| **Vulkan** | 100% GPU, 4.7 GB VRAM | 377 tok/s | **62.4 tok/s** | 4.0 s |
| CPU (`ollama`) | 100% CPU, 5.1 GB RAM | 82 tok/s | 6.6 tok/s | 36 s |

- `load_tensors: offloaded 29/29 layers to GPU`; footprint 4168 MiB weights + 224
  KV + 129 compute ≈ 4.5 GiB, well inside 8 GiB → room for larger context.
- Vulkan decode is **~9.5× CPU**. 62 tok/s on a 7B coding model is comfortably
  interactive. Journal clean throughout — no `amdgpu` fault/timeout/reset.
- **No `rocmOverrideGfx`, no `HSA_OVERRIDE`** — Vulkan runs native gfx1010 code,
  sidestepping the entire attempt-1 failure mode.

**Verdict — friction #1 RESOLVED: Vulkan.** ROCm rejected; CPU is a ~10× slower
fallback that also spends 5 GB of the 16 GB RAM budget. This also settles:
- **friction #4 (RAM):** weights live in VRAM, so host RAM stays free — the tight
  16 GB is a non-issue while on GPU. ✔
- **friction #5 (model):** `qwen2.5-coder:7b` Q4 confirmed — 4.5 GB, 62 tok/s. A
  fine default; no need to agonize. Can still try DeepSeek-Coder-6.7B later.
- Residual: finding #2 (a GPU-compute *hang* would still freeze the desktop) is now
  low-probability on the RADV path, but not zero. Acceptable.

**Config to commit:** the nixpkgs `services.ollama` module dropped the old
`acceleration` enum — just set `package = pkgs.ollama-vulkan`. The systemd unit
already adds `SupplementaryGroups = ["render"]` for `/dev/dri/render*` + `/dev/kfd`
access, so no manual group wiring. No gfx override option touched.

## Next-actions for exploration

- [x] ~~ROCm + gfx override~~ — **rejected 2026-07-30**, hangs the GPU / freezes
      the desktop (see spike log). gfx1010 can't safely run gfx1030 kernels.
- [x] **Spike GPU inference (gates everything)** — **DONE 2026-07-31, Vulkan wins**
      (62 tok/s decode vs 6.6 CPU, full offload, stable). Resolves friction #1/#4/#5.
- [ ] **Commit `services.ollama` (Vulkan) to `hosts/athena/default.nix`** and
      `nixos-rebuild switch`; confirm the daemon serves on `:11434`.
- [ ] **Verify the credential boundary:** run Gondolin, confirm from inside the VM
      that `~/.pi/agent/auth.json` / the API key is unreachable, and check whether
      tool-side network egress can be restricted.
- [ ] Stand up the two-entry `models.json`; confirm `/model` switching works with
      tools sandboxed in both modes.
- [ ] Decide + implement the reproducibility mechanism for the extension install.
- [ ] Establish a lightweight **output-verification** habit (the axis the sandbox
      can't cover) — this is the user's stated non-negotiable.
- [ ] If validated, record the decision as **ADR-0009 (Gondolin over Plain Docker
      for agent sandboxing)**, mirroring ADR-0006's shape.

## Related

- ADR-0006 (colima over Docker Desktop) — the container posture this extends; the
  eventual ADR-0009 mirrors its form.
- ADR-0004 (per-project devShells over global toolchains) — informs where Node
  lives (global thin fallback here, per CONTEXT.md, since Pi is cross-project).
- #16 Deep work flow — a local agent that never leaves the machine fits the
  "turn off / bounded" ethos; possible future tie-in.
- <no ADR yet — pending the GPU + credential-boundary spikes above>
