#############################################
#####                       __ _ _      #####
##                         / _(_) |        ##
##     _____ __  _ __ ___ | |_ _| | ___    ##
##    |_  / '_ \| '__/ _ \|  _| | |/ _ \   ##
##     / /| |_) | | | (_) | | | | |  __/   ##
##    /___| .__/|_|  \___/|_| |_|_|\___|   ##
##        | |                              ##
#####     |_|                           #####
#############################################
# Login shell configuration, runs after     #
# /etc/zprofile but before ~/.zshrc
# ========================================= #

# Path
DOTFILES_BIN="$DOTFILES/scripts"
LOCAL_BIN="$HOME/.local/bin"
DOOM_BIN="$HOME/code/doomemacs/bin"

export PATH="$DOTFILES_BIN:$LOCAL_BIN:$DOOM_BIN:$CARGO_HOME/bin:$GOPATH/bin:$PATH"
