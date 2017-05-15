# .dotfiles

*Features:*

- `zsh`
- `vim`
- `tmux`
- `spacemacs`
- `hammerspoon`
- and some custom scripts...

For now, these are my MacOS specific dotfiles. More OSs may come in the future. Feel free to use 'em!


## Installation

These steps assume you're setting up a new machine. However, the install script is non-destructive and can be run multiple times for updates or tweaks.

1. Download this repo with: `curl -L https://github.com/zachfedor/dotfiles/archive/master.zip -o ~/dotfiles.zip`
2. Unzip it: `unzip ~/dotfiles.zip`
3. Clean up: `mv ~/dotfiles-master ~/.dotfiles && rm ~/dotfiles.zip`
4. Run the installer: `.dotfiles/install.sh`

_Note: If the installer doesn't run, make sure the file's permissions allow it to be executed._

The installer will:

1. backup any existing dotfiles
2. create symlinks to new dotfiles
3. switch to `zsh` and install `oh-my-zsh`
4. install package managers: `homebrew`, `npm`, and `gem`
5. 
