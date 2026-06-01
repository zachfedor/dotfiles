# home-manager flake: one config end-to-end on macOS (tracer)

Status: done

## What to build

The tracer bullet for ADR-0001. Stand up a minimal Nix flake with home-manager
on macOS that places exactly one existing config (e.g. `zshrc`) via passthrough
(`home.file."...".source = ./zshrc`). Prove the whole pipeline end-to-end before
widening. `install.sh` is retired only for that one path.

## Decisions (grilled)

- **home-manager mode:** nix-darwin + home-manager (system + user together), not
  standalone HM. (Note: heavier than ADR-0001's deferred nix-darwin; chosen by
  owner anyway.)
- **First config:** `gitconfig` (self-contained, safe, easy to verify; avoids
  touching the live shell mid-session).
- **Placement:** passthrough `source = ./gitconfig` per ADR-0001.

## Prerequisite (blocking)

- **Nix is not installed on this Mac.** Install via Determinate Systems installer
  (flakes on by default, proper macOS `/nix` volume): user runs
  `curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install`.
  Flake identity: user=`zach`, host=`hestia`, system=`aarch64-darwin`.

## Acceptance criteria

- [x] `flake.nix` builds via nix-darwin + home-manager; `nix flake check` passes
- [x] `gitconfig` placed by home-manager passthrough (`home.file.".gitconfig".source
      = ./gitconfig`); `~/.gitconfig` → nix store, git identity intact
- [x] Edits in repo re-apply on rebuild (passthrough = verbatim content)
- [x] Old `install.sh` `~/.gitconfig` symlink superseded by HM-managed one
- [x] Rebuild command documented (below)

## Blocked by

None - can start immediately

## Comments

Done. `flake.nix` defines `darwinConfigurations.hestia` (nix-darwin +
home-manager module). Tracer = `gitconfig` via passthrough. Verified:
`~/.gitconfig` → `/nix/store/...hm_gitconfig`, identity Zach Fedor intact,
`flake.lock` pinned.

**Gotchas hit (record for #04/#05 + new machines):**
- **Determinate Nix coexistence:** set `nix.enable = false` in nix-darwin — both
  managing nix/daemon = conflict.
- **`/etc/zshenv` collision:** Determinate installer's `/etc/zshenv` blocked
  activation ("Unexpected files in /etc"). Fix: `sudo mv /etc/zshenv
  /etc/zshenv.before-nix-darwin`, re-run. nix-darwin installs its own
  (sources `set-environment`, adds `/run/current-system/sw/bin` to PATH).
- **zsh `#` glob:** `EXTENDED_GLOB` (zim) breaks unquoted `~/.dotfiles#hestia`.
  Quote: `"$HOME/.dotfiles#hestia"`.
- **Bootstrap needs sudo + TTY:** first activation via
  `sudo nix run nix-darwin -- switch --flake "$HOME/.dotfiles#hestia"` in a real
  terminal (not the agent's `!`, no TTY for password).

**Future rebuilds (post-bootstrap, new shell):**
```
darwin-rebuild switch --flake ~/.dotfiles#hestia
```

## Notes

- **direnv**: consider adding `direnv` + nix-direnv during the Nix work so a
  `.envrc` with `use flake` auto-loads a project's toolchain on `cd`. Strong
  synergy with the flake; hook into zsh (`eval "$(direnv hook zsh)"`) and place
  both via home-manager (carry into #04). Deferred from the issue-02 zsh pass —
  not wanted as a standalone shell addition, only alongside Nix.
