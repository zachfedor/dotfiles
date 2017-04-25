#!/bin/bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
# courtesy of: http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/
############################

########## Variables

DOTFILES_DIR=~/.dotfiles                    # dotfiles directory
BACKUP_DIR=~/.dotfiles_old             # old dotfiles backup directory

# list of files/folders to symlink in homedir
FILES="bash_profile bashrc dir_colors emacs.d hammerspoon spacemacs tmux.conf vimrc"

##########

# create dotfiles_old in homedir
echo "Creating $BACKUP_DIR for backup of any existing dotfiles in ~"
mkdir -p $BACKUP_DIR
echo "...done"

# change to the dotfiles directory
echo "Changing to the $DOTFILES_DIR directory"
cd $DOTFILES_DIR
echo "...done"

# TODO:
#   - split the backup and symlinking into two steps
#   - add exception catching
# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks
for FILE in $FILES; do
    echo "Moving any existing dotfiles from ~ to $BACKUP_DIR"
    mv ~/.$FILE $BACKUP_DIR/
    echo "Creating symlink to $FILE in home directory."
    ln -s $DOTFILES_DIR/$file ~/.$FILE
done


###### vim setup

# install vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# create vim backup and swap directory
mkdir -p ~/.vim/tmp


####### brew functions

fancy_echo() {
  local fmt="$1"; shift

  # shellcheck disable=SC2059
  printf "\n$fmt\n" "$@"
}

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

case "$SHELL" in
  */zsh) : ;;
  *)
    fancy_echo "Changing your shell to zsh ..."
      chsh -s "$(which zsh)"
    ;;
esac

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
cask_install 'steam'
cask_install 'toggldesktop'
cask_install 'vagrant'
cask_install 'virtualbox'
cask_install 'vlc'


# TODO: create npm_install function
# npm_install 'astrum'
# npm_install 'compass'
# npm_install 'create-react-app'
# npm_install 'eslint'
# npm_install 'http-server'

# TODO: create gem_install function
# gem_install 'bundler'
# gem_install 'jekyll'

# TODO: create git_clone function, take a username arg?
# git_clone 'chriskempson' 'base16-iterm2'
# git_clone 'chriskempson' 'base16-shell'
# git_clone 'reactjs' 'redux'
# git_clone 'ws' 'ws'
