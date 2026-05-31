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
# PROMPT TWEAKS
# ------------
# The `minimal` theme puts the working directory in the RIGHT prompt, which is
# easy to lose on wide screens. Move it to the left, ordered status > dir >
# vi-mode for consistency. Spliced before the keymap token (kept dim grey 244,
# trimmed to the last 2 dirs by prompt-pwd) so theme glyph changes survive. Git
# status stays on the right.
PS1=${PS1/'$(_prompt_mnml_keymap)'/'%F{244}$(prompt-pwd)%f $(_prompt_mnml_keymap)'}
RPS1='${(e)git_info[rprompt]}%f'

#
# ALIASES
# -------
[[ -f ~/.aliases ]] && source ~/.aliases

#
# AUTOCOMPLETION
# --------------------
# Stellar CLI
command -v stellar >/dev/null && source <(stellar completion --shell zsh)

#
# ZSH SETTINGS
# ----------
# Vim mode
bindkey -v
export KEYTIMEOUT=1

# Save a lot of history
HISTSIZE=1000000
SAVEHIST=1000000

# History search in vi insert mode (vi keymaps drop zsh's default Ctrl-R, so
# rebind it here). Pattern variants treat input as a glob (e.g. git*push).
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward
# Free Ctrl-S from terminal flow control (XOFF) so the forward bind works
unsetopt FLOW_CONTROL
# Vim-idiomatic history search from normal mode (/ forward, ? backward)
bindkey -M vicmd '?' history-incremental-pattern-search-backward
bindkey -M vicmd '/' history-incremental-pattern-search-forward

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
