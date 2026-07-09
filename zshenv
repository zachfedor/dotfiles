#############################################
#####           _                       #####
##             | |                         ##
##      _______| |__   ___ _ ____   __     ##
##     |_  / __| '_ \ / _ \ '_ \ \ / /     ##
##      / /\__ \ | | |  __/ | | \ V /      ##
##     /___|___/_| |_|\___|_| |_|\_/       ##
#####                                   #####
#############################################
# Shell environment configuration. Not for  #
# interactive shells, use .zshrc instead.   #
# ========================================= #

# Editor
export EDITOR="vim"
export VISUAL="emacs"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

# Dotfile directory
export DOTFILES="$HOME/.dotfiles"
export DOOMDIR="$DOTFILES/doom"

# Installed binaries
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export DOCKER_HOST="unix://$HOME/.config/colima/default/docker.sock"
export GOPATH="$XDG_DATA_HOME/go"
