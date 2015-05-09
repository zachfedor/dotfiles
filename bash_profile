# ~/.bash_profile

# executed by the command interpreter for login shells
# replaces .bash_login and .profile

if [ -r ~/.bashrc ];
then
    source ~/.bashrc;
fi

export PATH=$PATH:/usr/local/mysql/bin
