#!/bin/bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

DOTFILES_DIR="$HOME"/.dotfiles                    # dotfiles directory
BACKUP_DIR="$HOME"/.dotfiles_old             # old dotfiles backup directory

# list of files/folders to symlink in homedir
FILES="bash_profile bashrc dir_colors emacs.d gitconfig hammerspoon spacemacs tmux.conf vimrc vimrc_background zshrc zshrc.local"

##########

fancy_echo() {
    local fmt="$1"; shift

    # shellcheck disable=SC2059
    printf "\n$fmt\n" "$@"
}

backup_dotfile() {
    BACKUP="$HOME"/."$1"

    # create backup directory if it doesn't already exist
    if [ ! -d "$BACKUP_DIR" ]; then
        fancy_echo "Creating backup directory: %s" "$BACKUP_DIR"
        mkdir -p "$BACKUP_DIR"
    fi

    # if file exists and is NOT already a symlink, back it up
    if [ -e "$BACKUP" ] && [ ! -L "$BACKUP" ]; then
        fancy_echo "Moving %s to backup directory" "$BACKUP"
        mv "$BACKUP" "$BACKUP_DIR"/
    fi
}

symlink_dotfile() {
    SOURCE="$DOTFILES_DIR"/"$1"
    TARGET="$HOME"/."$1"

    if [ -e "$SOURCE" ]; then
        fancy_echo "Creating symlink for: .%s" "$1"
        ln -s "$SOURCE" "$TARGET"
    else
        fancy_echo "Error: .%s doesn't exist in dotfile repo" "$1"
    fi
}

# TODO: move dotfile symlink to after other installs because some conflict (e.g. oh-my-zsh)
# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks
for FILE in $FILES; do
    backup_dotfile $FILE
    symlink_dotfile $FILE
done


###### vim setup

# install vim-plug
if [ ! -f "$HOME"/.vim/autoload/plug.vim ]; then
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# create vim backup and swap directory
if [ ! -d ~/.vim/tmp ]; then
    mkdir -p ~/.vim/tmp
fi


####### brew functions

brew_install_or_upgrade() {
    if brew_is_installed "$1"; then
        if brew_is_upgradable "$1"; then
            fancy_echo "Upgrading %s ..." "$1"
            brew upgrade "$@"
        else
            fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
        fi
    else
        fancy_echo "Installing %s ..." "$1"
        brew install "$@"
    fi
}

cask_install() {
    fancy_echo "Installing %s ..." "$1"
    brew cask install "$@"
}

brew_is_installed() {
    local name="$(brew_expand_alias "$1")"

    brew list -1 | grep -Fqx "$name"
}

brew_is_upgradable() {
    local name="$(brew_expand_alias "$1")"

    ! brew outdated --quiet "$name" >/dev/null
}

brew_expand_alias() {
    brew info "$1" 2>/dev/null | head -1 | awk '{gsub(/:/, ""); print $1}'
}

brew_tap() {
    brew tap "$1" 2> /dev/null
}


####### other functions

npm_install() {
    if npm_is_installed "$1"; then
        fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
    else
        fancy_echo "Installing %s ..." "$1"
        npm install -g "$@"
    fi
}

npm_is_installed() {
    npm list -g --depth=0 | grep -Fqx "$1"
}

check_rbenv() {
    if [ ! -d "$HOME"/.rbenv ]; then
        rbenv init
    fi

    # set ruby version to latest stable version
    if [ ! -d "$HOME"/.rbenv/versions/2.4.1 ]; then
        rbenv install 2.4.1
        rbenv global 2.4.1
    fi
}

gem_install() {
    check_rbenv

    if gem_is_installed "$1"; then
        fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
    else
        fancy_echo "Installing %s ..." "$1"
        gem install "$@"
    fi
}

gem_is_installed() {
    gem list | grep -Fqx "$1"
}

git_clone() {
    GIT_DIR="$HOME"/git 
    REPO_DIR="$GIT_DIR"/"$2"
    REPO_URL="https://github.com/$1/$2.git"

    if [ ! -d "$GIT_DIR" ]; then
        mkdir "$GIT_DIR"
    fi

    if [ ! -d "$REPO_DIR" ]; then
        fancy_echo "Cloning %s into %s" "$REPO_URL" "$REPO_DIR"
        git clone "$REPO_URL" "$REPO_DIR"
    fi
}

case "$SHELL" in
    */zsh) : ;;
    *)
        fancy_echo "Changing your shell to zsh ..."
        chsh -s "$(which zsh)"
        ;;
esac

###### setup ssh
SSH_DIR="$HOME"/.ssh 
SSH_CONFIG="$SSH_DIR"/config
if [ ! -d "$SSH_DIR" ]; then
    fancy_echo "Creating the .ssh directory..."
    mkdir "$SSH_DIR" 
fi
# backup ssh config if it exists and isn't already a symlink
if [ -f "$SSH_CONFIG" ] && [ ! -L "$SSH_CONFIG" ]; then
    fancy_echo "Backing up the old .ssh config..."
    mkdir "$BACKUP_DIR"/ssh
    mv "$SSH_CONFIG" "$BACKUP_DIR"/ssh/config
fi
if [ ! -e "$SSH_CONFIG" ]; then
    fancy_echo "Creating symlink for new .ssh config"
    ln -s "$HOME"/.dotfiles/ssh/config "$SSH_CONFIG"
fi


# install oh-my-zsh
if [ ! -d "$HOME"/.oh-my-zsh ]; then
    fancy_echo "Installing Oh-My-Zsh ..."
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

if ! command -v brew >/dev/null; then
    fancy_echo "Installing Homebrew ..."
    curl -fsS \
      'https://raw.githubusercontent.com/Homebrew/install/master/install' | ruby

    export PATH="/usr/local/bin:$PATH"
else
    fancy_echo "Homebrew already installed. Skipping ..."
fi

fancy_echo "Updating Homebrew formulas ..."
brew update

if ! command -v rcup >/dev/null; then
    brew_tap 'thoughtbot/formulae'
    brew_install_or_upgrade 'rcm'
fi

fancy_echo "Installing development software..."
brew_tap 'homebrew/dupes'
brew_tap 'homebrew/homebrew-php'
brew_tap 'caskroom/cask'
brew_tap 'caskroom/versions'

# tools
brew_install_or_upgrade 'editorconfig'
brew_install_or_upgrade 'git'
brew_install_or_upgrade 'gnu-tar'
brew_install_or_upgrade 'heroku'
brew_install_or_upgrade 'openssl'
brew_install_or_upgrade 'the_silver_searcher'
brew_install_or_upgrade 'tmux'
brew_install_or_upgrade 'tree'

# php
brew_install_or_upgrade 'php56'
brew_install_or_upgrade 'php56-xdebug'
brew_install_or_upgrade 'composer'
composer self-update

# javascript
brew_install_or_upgrade 'node'
brew_install_or_upgrade 'yarn'

# elm
brew_install_or_upgrade 'elm'

# ruby
brew_install_or_upgrade 'rbenv'
brew_install_or_upgrade 'ruby-build'

# python
brew_install_or_upgrade 'pyenv'
brew_install_or_upgrade 'pyenv-virtualenv'

# lisp
brew_install_or_upgrade 'guile'
brew_install_or_upgrade 'mit-scheme'

# lua
brew_install_or_upgrade 'lua'

# databases
brew_install_or_upgrade 'postgresql'

# media
brew_install_or_upgrade 'ffmpeg'
brew_install_or_upgrade 'gifsicle'


# programs
cask_install 'atom'
cask_install 'calibre'
cask_install 'dropbox'
cask_install 'filezilla'
cask_install 'firefoxdeveloperedition'
cask_install 'flux'
cask_install 'google-chrome'
cask_install 'hammerspoon'
cask_install 'iterm2-nightly'
cask_install 'kindle'
cask_install 'kobo'
cask_install 'openemu'
cask_install 'private-internet-access'
cask_install 'skyfonts'
cask_install 'skype'
cask_install 'steam'
cask_install 'toggldesktop'
cask_install 'vagrant'
cask_install 'virtualbox'
cask_install 'vlc'


# javascript packages
if ! command -v npm >/dev/null; then
    fancy_echo "npm isn't installed. Skipping ..."
else
    npm_install 'astrum'
    npm_install 'compass'
    npm_install 'create-react-app'
    npm_install 'eslint'
    npm_install 'http-server'
fi


# ruby packages
if ! command -v gem >/dev/null; then
    fancy_echo "gem isn't installed. Skipping ..."
else
    gem_install 'bundler'
    gem_install 'jekyll'
fi


# git repositories
if ! command -v git >/dev/null; then
    fancy_echo "git isn't installed. Skipping ..."
else
    git_clone 'chriskempson' 'base16-iterm2'
    git_clone 'chriskempson' 'base16-shell'
fi
