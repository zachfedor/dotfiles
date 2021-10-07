# ~/.bash_profile

# executed by the command interpreter for login shells
# replaces .bash_login and .profile

if [ -r ~/.bashrc ];
then
    source ~/.bashrc;
fi

export PATH=$PATH:/usr/local/mysql/bin
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
