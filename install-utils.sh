#!/bin/bash
############################
# Bootstrap utilities
############################


# Print with color
fancy_echo() {
  local fmt="$1"; shift

  # shellcheck disable=SC2059
  printf "\n$fmt\n" "$@"
}


# Create a backup of a dotfile in a dedicated directory
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


# Create a symlink in home directory of given file
symlink_dotfile() {
  SOURCE="$DOTFILES_DIR"/"$1"
  TARGET="$HOME"/."$1"

  # if the source file exists and the target is not already a symlink
  if [ -e "$SOURCE" ]; then
    if [ ! -L "$TARGET" ]; then
      fancy_echo "Creating symlink for: .%s" "$1"
      ln -s "$SOURCE" "$TARGET"
    else
      fancy_echo "Skipping: .%s is already a symlink" "$1"
    fi
  else
    fancy_echo "Error: .%s doesn't exist in dotfile repo" "$1"
  fi
}


# Install or upgrade program via Homebrew
brew_install() {
  if brew_is_installed "$1"; then
    if brew_is_upgradable "$1"; then
      fancy_echo "Upgrading %s ..." "$1"
      brew upgrade "$1"
    else
      fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
    fi
  else
    fancy_echo "Installing %s ..." "$1"
    brew install "$1"
  fi
}


# Install OSX application via Homebrew Cask
cask_install() {
  if brew_is_installed "$1"; then
    if brew_is_upgradable "$1"; then
      fancy_echo "Upgrading %s ..." "$1"
      brew upgrade --cask "$1"
    else
      fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
    fi
  else
    fancy_echo "Installing %s ..." "$1"
    brew install --cask "$1"
  fi
}


# Determine if program is installed via Homebrew
brew_is_installed() {
  local name="$(brew_expand_alias "$1")"

  brew list -1 | grep -Fqx "$name"
}


# Determine if program should be upgraded via Homebrew
brew_is_upgradable() {
  local name="$(brew_expand_alias "$1")"

  ! brew outdated --quiet "$name" >/dev/null
}


# Expand program alias into full formula
brew_expand_alias() {
  brew info "$1" 2>/dev/null | head -1 | awk '{gsub(/:/, ""); print $1}'
}


# Tap a non-standard Homebrew repository to access its formulas
brew_tap() {
  brew tap "$1" 2> /dev/null
}


# Install a program via Node's npm
npm_install() {
  if npm_is_installed "$1"; then
    if npm_is_outdated "$1"; then
      npm update -g "$@"
    else
      fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
    fi
  else
    fancy_echo "Installing %s ..." "$1"
    npm install -g "$@"
  fi
}

# Determine if a program is installed via Node's npm
npm_is_installed() {
  npm list -g --depth=0 | grep -Fqx "$1"
}

# Determine if a program needs updated via npm
npm_is_outdated() {
  npm outdated -g | grep -Fqx "$1"
}

# Initiate rbenv and set global Ruby to latest stable version 
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


# Install a program via Ruby's gem
gem_install() {
  check_rbenv

  if gem_is_installed "$1"; then
    fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
  else
    fancy_echo "Installing %s ..." "$1"
    gem install "$@"
  fi
}


# Determine if a program is installed via Ruby's gem
gem_is_installed() {
  gem list | grep -Fqx "$1"
}


# Clone a git repository from Github
# param : path to place clone
# param : URL of repository
git_clone() {
  BASE_DIR="$1"
  REPO_URL="$2"
  # extract project name from repo url
  TMP=${2##*/} # save substring after last '/'
  REPO_NAME=${TMP%.git} # save substring before '.git'
  REPO_DIR="$BASE_DIR"/"$REPO_NAME"

  if [ ! -d "$BASE_DIR" ]; then
    mkdir -p "$BASE_DIR"
  fi

  if [ ! -d "$REPO_DIR" ]; then
    fancy_echo "Cloning %s into %s" "$REPO_NAME" "$REPO_DIR"
    git clone "$REPO_URL" "$REPO_DIR"
  fi
}


