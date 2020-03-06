#!/bin/bash
# --------------------
# Bootstrap new OSX machine
#
# - switches shell to zsh and installs oh-my-zsh
# - installs programs via homebrew and language specific package maintainers
# - symlinks dotfiles from this source controlled repo to the home directory
# - sets up necessary environment for vim editor
# - sets up ssh configuration
# --------------------


# Functions and Variables
# --------------------
DOTFILES_DIR="$HOME"/.dotfiles      # dotfiles directory
BACKUP_DIR="$HOME"/.dotfiles_old    # old dotfiles backup directory
SSH_DIR="$HOME"/.ssh                # ssh 
SSH_CONFIG="$SSH_DIR"/config
# list of files/folders to symlink in homedir
FILES="bash_profile bashrc dir_colors emacs.d gitconfig hammerspoon spacemacs tmux.conf vimrc vimrc_background zshrc"

source "$DOTFILES_DIR"/install-utils.sh


# Setup ZSH
# --------------------
case "$SHELL" in
  */zsh) : ;;
  *)
    fancy_echo "Changing your shell to zsh ..."
    chsh -s "$(which zsh)"

    if [ ! -f "$HOME"/.zsh.local ]; then
      fancy_echo "TODO: Get a copy of your ~/.zsh.local for API tokens!!!"
    fi
    ;;
esac

if [ ! -d "$HOME"/.oh-my-zsh ]; then
  fancy_echo "Installing Oh-My-Zsh ..."
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
else
  fancy_echo "Oh-my-zsh already installed. Skipping..."
fi


# Install via Homebrew
# --------------------
if ! command -v brew >/dev/null; then
  fancy_echo "Installing Homebrew..."
  curl -fsS \
    'https://raw.githubusercontent.com/Homebrew/install/master/install' | ruby

  export PATH="/usr/local/bin:$PATH"
else
  fancy_echo "Homebrew already installed. Skipping..."
fi

fancy_echo "Tapping homebrew repos..."
brew_tap 'homebrew/homebrew-php'  # for all PHP related formulas
brew_tap 'caskroom/cask'          # for all macOS application formulas
brew_tap 'caskroom/versions'      # "
brew_tap 'thoughtbot/formulae'    # for all Thoughtbot formulas
brew_tap 'osx-cross/avr'          # for QMK build tools
brew_tap 'PX4/homebrew-px4'       # "

fancy_echo "Updating formulas..."
brew update

if ! command -v rcup >/dev/null; then
  brew_install_or_upgrade 'rcm'
fi

# tools
brew_install_or_upgrade 'docker'
brew_install_or_upgrade 'editorconfig'
brew_install_or_upgrade 'git'
brew_install_or_upgrade 'gnu-tar'
brew_install_or_upgrade 'heroku'
brew_install_or_upgrade 'openssl'
brew_install_or_upgrade 'p7zip'
brew_install_or_upgrade 'reattach-to-user-namespace'
brew_install_or_upgrade 'the_silver_searcher'
brew_install_or_upgrade 'tmux'
brew_install_or_upgrade 'tree'
brew_install_or_upgrade 'vim'

# php
brew_install_or_upgrade 'php56'
brew_install_or_upgrade 'php56-xdebug'
brew_install_or_upgrade 'composer'
composer self-update

# javascript
brew_install_or_upgrade 'node'
brew_install_or_upgrade 'yarn'

# ruby
brew_install_or_upgrade 'rbenv'
brew_install_or_upgrade 'ruby-build'

# python
brew_install_or_upgrade 'pyenv'
brew_install_or_upgrade 'pyenv-virtualenv'

# lisp
brew_install_or_upgrade 'guile'
brew_install_or_upgrade 'mit-scheme'

# databases
brew_install_or_upgrade 'postgresql'

# other
brew_install_or_upgrade 'elm'
brew_install_or_upgrade 'ffmpeg'
brew_install_or_upgrade 'gifsicle'
brew_install_or_upgrade 'lua'

# QMK build tools
brew_install_or_upgrade 'avr-gcc'
brew_install_or_upgrade 'dfu-programmer'
brew_install_or_upgrade 'gcc-arm-none-eabi'
brew_install_or_upgrade 'avrdude'
brew_install_or_upgrade 'teensy_loader_cli'


# Install via Homebrew Cask
# --------------------
cask_install 'atom'
cask_install 'calibre'
cask_install 'discord'
cask_install 'dropbox'
cask_install 'filezilla'
cask_install 'firefoxdeveloperedition'
cask_install 'flux'
cask_install 'google-chrome'
cask_install 'hammerspoon'
# cask_install 'xquartz' # dependency of inkscape
# cask_install 'inkscape'
cask_install 'iterm2-nightly'
cask_install 'jdiskreport'
# cask_install 'karabiner'
# cask_install 'keycastr'
cask_install 'kindle'
cask_install 'kobo'
cask_install 'openemu'
# cask_install 'openmw'
cask_install 'private-internet-access'
cask_install 'slack'
cask_install 'skyfonts'
cask_install 'skype'
cask_install 'steam'
# cask_install 'toggldesktop'
cask_install 'transmission'
cask_install 'tunnelblick'
cask_install 'vagrant'
cask_install 'virtualbox'
cask_install 'vlc'


# Install via NPM
# --------------------
if ! command -v npm >/dev/null; then
  fancy_echo "npm isn't installed. Skipping ..."
else
  # npm configuration
  npm config set init-author-name 'Zach Fedor'
  npm config set init-author-email 'zachfedor@gmail.com'
  npm config set init-author-url 'http://zachfedor.me'
  npm config set init-license 'MIT'
  npm config set init-version '0.0.1'

  # node versioning
  npm_install 'n'
  npm_install 'avn'
  npm_install 'avn-n'
  if ! command -v avn >/dev/null; then
    fancy_echo "Error: avn isn't installed. node versioning won't work."
  else
    avn setup
  fi

  npm_install 'astrum'
  npm_install 'compass'
  npm_install 'create-react-app'
  npm_install 'eslint'
  npm_install 'http-server'
fi


# Install via Gem
# --------------------
if ! command -v gem >/dev/null; then
  fancy_echo "gem isn't installed. Skipping ..."
else
  gem_install 'bundler'
  gem_install 'jekyll'
fi


# Clone Git Repositories
# --------------------
if ! command -v git >/dev/null; then
  fancy_echo "git isn't installed. Skipping ..."
else
  git_clone "$HOME/git" 'git@github.com:chriskempson/base16-iterm2.git'
  git_clone "$HOME/git" 'git@github.com:chriskempson/base16-shell.git'
  git_clone "$HOME/git" 'git@github.com:apprenticeharper/DeDRM_tools.git'

  git_clone "$HOME/git" 'git@github.com:zachfedor/qmk_firmware.git'
  git_clone "$HOME/code" 'git@github.com:zachfedor/zachfedor.github.io.git'
fi


# Symlink Dotfiles
# --------------------
for FILE in $FILES; do
  # move any existing dotfiles in homedir to dotfiles_old directory
  backup_dotfile $FILE
  # then create symlinks in homedir to my dotfiles
  symlink_dotfile $FILE
done


# Setup Vim
# --------------------
if [ ! -f "$HOME"/.vim/autoload/plug.vim ]; then
  # install vim-plug
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# create vim backup and swap directory
if [ ! -d ~/.vim/tmp ]; then
  mkdir -p ~/.vim/tmp
fi


# Setup SSH
# --------------------
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

