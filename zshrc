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

# zimfw framework path, provided by home-manager (nixpkgs zimfw on both macOS and
# NixOS — see home.nix). Sets ZIM_FW_INIT to the nix store path so this file
# stays path-free and OS-agnostic (replaced a uname brew/linuxbrew case).
ZIM_FW_INIT_FRAGMENT=${XDG_CONFIG_HOME:-${HOME}/.config}/zsh/zim-fw-init.zsh
[[ -f ${ZIM_FW_INIT_FRAGMENT} ]] && source ${ZIM_FW_INIT_FRAGMENT}

# Avoid nested git repos in `modules/`
zstyle ':zim:zmodule' use 'degit'

# Framework version is pinned by nixpkgs (issue 05a), so disable zim's self-update
# version check. Do NOT run `zimfw upgrade` (the framework is read-only in the nix
# store) — bump it with `nix flake update nixpkgs` instead. Modules still update
# with `zimfw update`.
zstyle ':zim' 'disable-version-check' 'true'

# Install missing modules and update ${ZIM_HOME}/init.zsh if missing or outdated.
if [[ ! ${ZIM_HOME}/init.zsh -nt ${ZIM_CONFIG_FILE:-${ZDOTDIR:-${HOME}}/.zimrc} ]]; then
  source "${ZIM_FW_INIT}" init
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
# DIRENV
# ------
# Per-project envs via nix-direnv (`use flake`). HM's programs.direnv only hooks
# HM-managed shells; our zsh is passthrough, so hook it manually here.
command -v direnv >/dev/null && eval "$(direnv hook zsh)"

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
