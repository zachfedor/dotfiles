# New host setup

Rebuild reproduces most config. These bits stay manual: Nix bootstrap, machine
identity (hostname/keys/password), Doom (hand-managed by design, [ADR-0002](adr/0002-doom-native-profiles.md)).

**Rule:** machine hostname must match its flake attr key (`hestia`/`athena`). The
`rebuild` helper derives it from `hostname -s`.

## 1. Nix + clone dotfiles

```sh
# a. macOS only — install Nix (NixOS has it built in):
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# b. clone this repo to ~/.dotfiles (public, HTTPS read needs no auth;
#    nix-shell provides git if the machine lacks it):
nix-shell -p git --run 'git clone https://github.com/zachfedor/dotfiles.git ~/.dotfiles'
```

## 2. Add host to flake

NixOS: adopt the installer config (correct bootloader/fs/locale):

```sh
mkdir -p ~/.dotfiles/hosts/<host>
cp /etc/nixos/hardware-configuration.nix ~/.dotfiles/hosts/<host>/
cp /etc/nixos/configuration.nix          ~/.dotfiles/hosts/<host>/default.nix
```

Add the `nixosConfigurations.<host>` / `darwinConfigurations.<host>` entry in
`flake.nix` (copy an existing host). In `hosts/<host>/default.nix` set the user
account (§4) + `nix.settings.experimental-features = [ "nix-command" "flakes" ]` +
`services.openssh` (`openFirewall = true`, your key in `authorizedKeys`).

**Version match:** flake pins one nixpkgs release for all hosts. New machine on a
different release → bump the flake to match (else option-name skew, e.g. GNOME
options moved out of `services.xserver` between releases).

## 3. First switch

Quote the flake ref (`#` is a glob under zsh `EXTENDED_GLOB`); absolute path (under
`sudo`, `~` = root's home):

```sh
# NixOS (first time may need the flag):
sudo nixos-rebuild switch --flake '/home/<user>/.dotfiles#<host>' \
  --extra-experimental-features 'nix-command flakes'
# macOS first time:
sudo nix run nix-darwin -- switch --flake '/Users/<user>/.dotfiles#<host>'
```

Then use `rebuild` (in `aliases`).

## 4. Identity (manual)

- NixOS user in `hosts/<host>/default.nix`: `isNormalUser`, groups
  `wheel networkmanager audio video`, `shell = pkgs.zsh`; + `programs.zsh.enable`
  and `programs.zsh.enableCompletion = false` (zim owns completion).
- `passwd` (or `hashedPassword` — never plaintext in the flake).
- `ssh-keygen -t ed25519`; keys stay out of the flake. Public key → GitHub / other
  hosts' `authorizedKeys`.
- Shell change needs **re-login** — GNOME keeps old `$SHELL` until logout/in.

## 5. Doom (imperative)

```sh
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/code/doomemacs
rm -f ~/.emacs.d        # if a pre-existing symlink — HM can't back up symlink-to-dir
rebuild                 # HM creates ~/.emacs.d → ~/code/doomemacs + ~/.config/doom
~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom env # GUI Emacs inherits nix PATH
~/.emacs.d/bin/doom doctor
```

Doctor warnings about language tools are optional. Linux hard deps (python, X11
clipboard) already in `home.nix`. After config changes later: `doom sync`.

## 6. Manual apps & sign-ins (out of nix scope)

Nix declares installs; **app-internal state stays each app's own cloud sync** (no
profiles/extensions/bookmarks in the flake — issue 10 / ADR-0004 boundary). After
a rebuild, sign in once per app:

- **Firefox** → Firefox Sync (bookmarks, extensions, prefs)
- **Brave** → Brave Sync (optional)
- **1Password** → account sign-in; enable browser unlock + `op` CLI integration
- **Slack / Discord / Telegram** → workspace/account login
- **Syncthing** → already declarative; just approve the new device on peers (#06)
- **Tailscale** → `sudo tailscale up` (CLI-only on hestia, no menubar)
- **Anki** → AnkiWeb sync

**Vendored apps** (vendor installers, *not* brew/nix — reinstall by hand; macOS-only):

- Music: Ableton Live, GarageBand (MAS), Melodics, Akai/inMusic Software Center,
  Studio Instrument Collection
- Hardware: Logi Options / LogiTune (Logitech)
- Printing: PaperCut Hive

**Containers (macOS):** `colima start` once after first rebuild — colima provides
the Linux VM the `docker` CLI talks to (ADR-0006). athena uses the native daemon.

## 7. Deferred

- `base16-shell` terminal colors: manual clone to `~/.config/base16-shell` until
  nix-managed (#08).
- Static IP: reserve on router, then add `Host <host>` to `programs.ssh`.

## Gotchas

| Symptom | Fix |
|---------|-----|
| `file 'nixos-config' was not found` | missing `--flake` |
| flake path errors under `sudo` | use absolute path, not `~` |
| `#host` glob/completion errors | quote the flake ref |
| ssh "No route to host", ping ok | `services.openssh.openFirewall = true` |
| terminal still bash after shell change | log out/in |
| GUI Emacs white theme + dead leader | missing font family (`hack-font`/`merriweather-sans`); check `fc-scan` |
| zim "completion already initialized" | `programs.zsh.enableCompletion = false` |
| `zimfw upgrade` stale path post-migration | one-time `rm ~/.zim/init.zsh` |

**Git flow:** only hosts with SSH-remote push can push; others pull read-only. Move
commits off a pull-only host via the LAN ssh link:
`git fetch ssh://<user>@<ip>/home/<user>/.dotfiles <branch>` then push.
