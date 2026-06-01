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

# Global Functions and Variables
# --------------------
DOTFILES_DIR="$HOME"/.dotfiles   # dotfiles directory
BACKUP_DIR="$HOME"/.dotfiles_old # backup directory for old dotfiles
# list of files/folders to symlink in homedir
FILES="aliases gitconfig gitignore_global hammerspoon tmux.conf vimrc vimrc_background zimrc zprofile zshenv zshrc"

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

# Setup Homebrew
# --------------
# check if homebrew is installed
if ! command -v brew >/dev/null; then
    fancy_echo "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    fancy_echo "Homebrew already installed. Skipping..."
fi

# access alternative formulas
fancy_echo "Tapping homebrew repos..."
TAPS=(
    'chrokh/tap'       # for Base16 repos
    'osx-cross/avr'    # for QMK build tools
    'PX4/homebrew-px4' # "
    'heroku/brew'
    # 'railwaycat/emacsmacport' # for Mitsuharu's emacs-mac port
    'd12frosted/emacs-plus' # for Emacs 29 with native comp support
    'thoughtbot/formulae'   # for Thoughtbot formulas
    'clojure/tools'         # for regular clojure releases
    'clojure-lsp/brew'
)
for tap in "${TAPS[@]}"; do
    brew_tap $tap
done

# make sure everything is updated
brew update

if ! command -v rcup >/dev/null; then
    brew_install 'rcm'
fi

# Install via Homebrew
# --------------------
FORMULAS=(
    # dependencies
    'git'
    'coreutils'   # for doom-emacs
    'fd'          # for doom-emacs
    'ripgrep'     # for doom-emacs
    'tree-sitter' # for emacs-plus@29+

    # tools
    'editorconfig'
    'emacs-plus@30'
    'gnu-tar'
    'heroku'
    'htop'
    'neovim'
    'mise'
    'openssl'
    'pandoc'
    'p7zip'
    'the_silver_searcher'
    'tmux'
    'tree'
    'vim'

    # html/css/javascript
    'tidy-html5'

    # lisp
    'clojure/tools/clojure'
    'clojure-lsp/brew/clojure-lsp-native'
    'leiningen'
    'guile'
    # 'mit-scheme'

    # shell
    'shellcheck'

    # databases
    # 'mongodb-community'
    'postgresql'
    'redis'
    'sqlite'

    # other
    # 'elm'
    'ffmpeg'
    'gifsicle'
    'lua'
    'nethack'
    'shfmt'
    'zork'

    # QMK build tools
    # 'avr-gcc'
    # 'dfu-programmer'
    # 'gcc-arm-none-eabi'
    # 'avrdude'
    # 'teensy_loader_cli'
)
for formula in "${FORMULAS[@]}"; do
    brew_install $formula
done

# Install via Homebrew Cask
# --------------------
CASKS=(
    # Applications
    'calibre'
    'discord'
    'dropbox'
    'firefox'
    'google-chrome'
    'hammerspoon'
    'iterm2'
    'jdiskreport'
    'kobo'
    # 'notion'
    'obs'
    'openemu'
    'slack'
    'steam'
    'transmission'
    'tunnelblick'
    'ubersicht'
    'virtualbox'
    'visual-studio-code'
    'vlc'

    # Fonts
    'font-cormorant'
    'font-cutive-mono'
    'font-et-book'
    'font-fira-code-nerd-font'
    'font-fira-mono-nerd-font'
    'font-fira-sans'
    'font-hack-nerd-font'
    'font-inconsolata-nerd-font'
    'font-lato'
    'font-lora'
    'font-merriweather'
    'font-merriweather-sans'
    'font-roboto'
    'font-roboto-mono'
    'font-source-code-pro'
    'font-source-sans-pro'
    'font-source-serif-pro'
    'font-vt323'
)
for cask in "${CASKS[@]}"; do
    cask_install $cask
done

# Install via NPM
# --------------------
if ! command -v npm >/dev/null; then
    fancy_echo "npm isn't installed. Skipping ..."
else
    # update npm
    npm install -g npm

    # npm configuration
    npm config set init-author-name 'Zach Fedor'
    npm config set init-author-email 'zachfedor@gmail.com'
    npm config set init-author-url 'http://zachfedor.me'
    npm config set init-license 'MIT'
    npm config set init-version '0.0.1'

    # node versioning
    fancdy_echo "Installing 'n' for managing node versions"
    npm_install 'n'
    if ! command -v n >/dev/null; then
        fancy_echo "Installation failed. Skipping 'n' setup..."
    else
        # Steps from https://github.com/tj/n#installation
        # Make cache folder to hold node versions and take ownership
        sudo mkdir -p /usr/local/n
        sudo chown -R $(whoami) /usr/local/n
        # Make sure Node.js install destination folders exist and take ownership
        sudo mkdir -p /usr/local/bin /usr/local/lib /usr/local/include /usr/local/share
        sudo chown -R $(whoami) /usr/local/bin /usr/local/lib /usr/local/include /usr/local/share

        # Using n, install latest version of node
        n latest
    fi
    npm_install 'avn'
    npm_install 'avn-n'
    if ! command -v avn >/dev/null; then
        fancy_echo "Error: avn isn't installed. node versioning won't work."
    else
        avn setup
    fi

    npm_install 'yarn'
    npm_install 'astrum'
    npm_install 'eslint'
    npm_install 'http-server'
    npm_install 'prettier'
    npm_install 'typescript'
    npm_install 'typescript-language-server'
fi

# Install Lisp tools
# --------------------
# Install or update cljfmt, a formatter for Clojure used by tree-sitter in Emacs and Leiningen projects
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/weavejester/cljfmt/HEAD/install.sh)"

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
# RETIRED (issue 04a): dotfile placement is now declarative via Home Manager
# (see home.nix). Do NOT recreate symlinks here — it would collide with the
# HM-managed ones. The FILES loop, vim-plug bootstrap, and ~/.vim/tmp creation
# are gone; vim-plug and the swap/backup dirs are handled by the vim/neovim
# config work in issue 04b.

# Setup SSH
# --------------------
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
    ln -s "$DOTFILES_DIR"/ssh/config "$SSH_CONFIG"
fi
# generate ssh keys if they don't exist
# see: https://docs.github.com/en/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
if [[ ! $(compgen -G "${SSH_DIR}/id_*") ]]; then
    # generate keys
    ssh-keygen -t ed25519 -C "zachfedor@gmail.com"
    # start background ssh agent
    eval "$(ssh-agent -s)"
    fancy_echo "NOTE: Add new key to .ssh/config to allow keychain storage!"
    # add passphrase to keychain
    ssh-add -K ~/.ssh/id_ed25519
    # see: https://docs.github.com/en/github/authenticating-to-github/adding-a-new-ssh-key-to-your-github-account
    fancy_echo "NOTE: Now add new key to GitHub etc."
fi

# Setup Applications
# --------------------

# Emacs
# Symlink brew installed emacs package to apps folder, removing prior version if one exists
# if [ -f /Applications/Emacs.app ]; then
#   rm -rf /Applications/Emacs.app
# fi
# osascript -e 'tell application "Finder" to make alias file to POSIX file "/opt/homebrew/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'

# Emacs - Doom
# TODO: update now that I've added Chemacs2 and multiple profiles
# if [ ! -d "$HOME"/.emacs.d ]; then
#     # Clone doom repo into emacs directory
#     git clone https://github.com/hlissner/doom-emacs "$HOME"/.emacs.d

#     # The following commands are the equivalent of running `doom install`, with the exception of
#     # creating a ~/.doom.d directory and copying the example config files because I should have
#     # already symlinked my custom config files to ~/.doom.d/ by this point
#     # first, sync doom with those custom configs, e.g. install packages defined in init.el
#     "$HOME"/.emacs.d/bin/doom sync
#     # then, ensure envvars are inherited by emacs regardless of where it gets launched
#     "$HOME"/.emacs.d/bin/doom env
#     # lastly, install necessary icon fonts
#     emacs --batch -f all-the-icons-install-fonts

#     fancy_echo "Doom Emacs Installed. You might want to run $(doom doctor) to check the installation."
# fi

# Übersicht
UBERSICHT_DIR="$HOME"/Library/Application\ Support/Übersicht/widgets
if [ -f "$UBERSICHT_DIR" ] && [ ! -L "$UBERSICHT_DIR" ]; then
    # create symlink for widget directory if it isn't already
    ln -s "$DOTFILES_DIR"/ubersicht/widgets $UBERSICHT_DIR
fi
