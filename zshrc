#############################################
#####               _                   #####
##                 | |                     ##
##          _______| |__  _ __ ___         ##
##         |_  / __| '_ \| '__/ __|        ##
##          / /\__ \ | | | | | (__         ##
##         /___|___/_| |_|_|  \___|        ##
#####                                   #####
#############################################
# Interactive shell configuration. If it's  #
# needed in a script, use .zshenv instead.  #
# ========================================= #

#
# ZIM CONFIGURATION
# -----------------
ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Avoid nested git repos in `modules/`
zstyle ':zim:zmodule' use 'degit'

# Pin zim:fw version, upgrade manually with `zimfw upgrade`
zstyle ':zim' 'disable-version-check' 'true'

# Install missing modules and update ${ZIM_HOME}/init.zsh if missing or outdated.
if [[ ! ${ZIM_HOME}/init.zsh -nt ${ZIM_CONFIG_FILE:-${ZDOTDIR:-${HOME}}/.zimrc} ]]; then
  case `uname` in
    Darwin)
      source /opt/homebrew/opt/zimfw/share/zimfw.zsh init
    ;;
    Linux)
      source /home/linuxbrew/.linuxbrew/opt/zimfw/share/zimfw.zsh init
    ;;
  esac
fi

# Initialize modules.
source ${ZIM_HOME}/init.zsh

#
# ALIASES
# -------
[[ -f ~/.aliases ]] && source ~/.aliases

#
# AUTOCOMPLETION
# --------------------
# Stellar CLI
source <(stellar completion --shell zsh)

#
# ZSH SETTINGS
# ----------
# Vim mode
bindkey -v
export KEYTIMEOUT=1

# Save a lot of history
HISTSIZE=1000000
SAVEHIST=1000000

# Keep history search commands available while in vim mode
# bindkey -M vicmd "?" history-incremental-pattern-search-backward
# bindkey -M vicmd "/" history-incremental-pattern-search-forward
# bindkey -M viins '\C-R' history-incremental-pattern-search-backward
# bindkey -M viins '\C-S' history-incremental-pattern-search-forward
# unsetopt FLOW_CONTROL # disable C-s/C-q in the editor
bindkey '\C-R' history-incremental-search-backward
bindkey '\C-S' history-incremental-search-forward

# Share same history across instances
setopt SHARE_HISTORY

# History expansion goes into the editor buffer first
setopt HIST_VERIFY

# Don't show duplicates in history search
setopt HIST_FIND_NO_DUPS

# Don't history commands beginning in space (consistent with bash)
setopt HIST_IGNORE_SPACE

# Allow comments in the shell
setopt INTERACTIVE_COMMENTS

# Fish-like shell autosuggestion performance tweak
ZSH_AUTOSUGGEST_MANUAL_REBIND=1

#
# BASE16 SHELL
# ------------
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
