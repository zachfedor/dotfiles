#################
#   ~/.bashrc   #
#################
#
# https://wiki.archlinux.org/index.php/Color_Bash_Prompt

# If not running interactively, don't do anything!
[[ $- != *i* ]] && return

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# Enable history appending instead of overwriting.
shopt -s histappend

case ${TERM} in
	xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
		PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
		;;
	screen)
		PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
		;;
esac

# Set CLICOLOR for Ansi colors in iTerm2
export CLICOLOR=1

# Color codes
    RS="\[\033[0m\]"    # reset
    HC="\[\033[1m\]"    # hicolor
    UL="\[\033[4m\]"    # underline
    INV="\[\033[7m\]"   # inverse background and foreground
    FBLK="\[\033[30m\]" # foreground black
    FRED="\[\033[31m\]" # foreground red
    FGRN="\[\033[32m\]" # foreground green
    FYEL="\[\033[33m\]" # foreground yellow
    FBLE="\[\033[34m\]" # foreground blue
    FMAG="\[\033[35m\]" # foreground magenta
    FCYN="\[\033[36m\]" # foreground cyan
    FWHT="\[\033[37m\]" # foreground white
    BBLK="\[\033[40m\]" # background black
    BRED="\[\033[41m\]" # background red
    BGRN="\[\033[42m\]" # background green
    BYEL="\[\033[43m\]" # background yellow
    BBLE="\[\033[44m\]" # background blue
    BMAG="\[\033[45m\]" # background magenta
    BCYN="\[\033[46m\]" # background cyan
    BWHT="\[\033[47m\]" # background white

# sanitize TERM:
safe_term=${TERM//[^[:alnum:]]/?}
match_lhs=""

[[ -f ~/.dir_colors ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs} ]] \
	&& type -P dircolors >/dev/null \
	&& match_lhs=$(dircolors --print-database)

if [[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] ; then
	# we have colors :-)

	# Enable colors for ls, etc. Prefer ~/.dir_colors
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

    # Grabs the current git branch for prompt
    function current_git_branch {
        BRANCH="$(git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})"
        if [ "$BRANCH" == "" ]
        then
            echo "-"
        else
            echo "$BRANCH"
        fi
    }

    PS1="$RS[$FCYN\u$RS][$FCYN\w$RS][$FCYN\$(current_git_branch)\[$RESET\]$RS]\n[$FCYN>$RS] "
    #PS1="$RS[$FCYN\u$RS][$FCYN\w$RS]\n[$FCYN>$RS] "


	if [ "$TERM" == xterm ]
    then
        alias ls="ls --color=auto";
    elif [ "$TERM" == rxvt-unicode-256color ]
    then
        alias ls="ls --color";
    else
        alias ls="ls";
    fi
	alias dir="dir --color=auto"
	alias grep="grep --color=auto"

    # git aliases
    alias ga="git add "
    alias gA="git add -A "
    alias gc="git commit "
    alias gcm="git commit -m "
    alias gcam="git commit -a -m "
    alias gs="git status "
    alias gpush="git push "
    alias gpull="git pull "
    alias gpullr="git pull --rebase "
    alias gb="git branch "
    alias gch="git checkout "

else
	# show root@ when we do not have colors
	PS1="\u@\h \w \$([[ \$? != 0 ]] && echo \":( \")\$\" 
fi

PS2="> "
PS3="> "
PS4="+ "

# Try to keep environment pollution down, EPA loves us.
unset safe_term match_lhs

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Try to enable the "Command not found" hook ("pacman -S pkgfile" to install it).
# See also: https://wiki.archlinux.org/index.php/Bash#The_.22command_not_found.22_hook
[ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash

# Add RVM to PATH for scripting
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting


# Aliases
alias ..='cd ..'
alias ...="cd ../.."
alias la="ls -a"
alias ll="ls -l"
alias lla="ls -la"
#alias emacs='emacsclient'
alias gngb="gngb -os"

# Functions
function cdtheme {
    dir=${PWD##*/}
    cd wp-content/themes/$dir
}

function stopMAMP {
    if test -f /Applications/MAMP/Library/logs/httpd.pid; then
        echo "Stopping Mamp Apache Server..."
        /Applications/MAMP/Library/bin/apachectl -f"/Applications/MAMP/conf/apache/httpd.conf" -k stop
    fi
    if test -f /Applications/MAMP/tmp/mysql/mysql.pid; then
        echo "Stopping MAMP MySQL Server..."
        /bin/kill `cat /Applications/MAMP/tmp/mysql/mysql.pid`
    fi
}

function startMAMP { 
    echo "Starting MAMP Apache Server..."
    /Applications/MAMP/Library/bin/apachectl -f"/Applications/MAMP/conf/apache/httpd.conf" -k start

    echo "Starting MAMP MySQL Server..."
    /Applications/MAMP/Library/bin/mysqld_safe --defaults-file=/Applications/MAMP/tmp/mysql/my.cnf --user=mysql --port=MAMP_MysqlPort_MAMP --socket=/Applications/MAMP/tmp/mysql/mysql.sock --pid-file=/Applications/MAMP/tmp/mysql/mysql.pid --log-error="/Applications/MAMP/logs/mysql_error_log.err" --tmpdir=/Applications/MAMP/tmp/mysql/tmpdir --datadir=/Library/Application\ Support/appsolute/MAMP\ PRO/db/mysql
}

function mamptomagento {
    dir=/Applications/MAMP/conf/apache
    rm $dir/httpd.conf
    ln -s $dir/httpd-magento.conf $dir/httpd.conf

    stopMAMP
#    startMAMP

    echo "Happy Magento Developing!"
}
function mamptowordpress {
    dir=/Applications/MAMP/conf/apache
    rm $dir/httpd.conf
    ln -s $dir/httpd-wordpress.conf $dir/httpd.conf

    stopMAMP
#    startMAMP

    echo "Happy Wordpress Developing!"
}
