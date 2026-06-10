# Home Manager user configuration for zach.
#
# Passthrough-first (ADR-0001): existing dotfiles are placed verbatim from this
# repo. Content stays plain text, edited in-repo; Home Manager owns placement.
# Promote a config to a native module (programs.*) only when cross-OS branching
# or install/config coupling pays off.
#
# Placement methods used here:
#   - Store passthrough (`source = ./file`): file is copied into the read-only
#     nix store and symlinked in. Reproducible/pinned; edits apply on the next
#     `darwin-rebuild switch`. This is the default and the "nix way".
#   - Out-of-store symlink (`mkOutOfStoreSymlink`): symlink points straight at
#     the repo file; edits apply instantly, no rebuild. Used ONLY for ~/.config/doom
#     so the Emacs GUI (no DOOMDIR) and CLI (DOOMDIR) never diverge between
#     rebuilds (see ADR-0002 / issue 01).

{ config, lib, pkgs, ... }:

let
  # Derive from the HM-managed home dir so this is correct on both macOS
  # (/Users/zach) and NixOS (/home/zach).
  dotfiles = "${config.home.homeDirectory}/.dotfiles";
in {
  home.stateVersion = "25.05";

  # --- CLI packages (issue 04d; migrated from install.sh FORMULAS, curated) ---
  # As each tool lands here it is removed from install.sh AND `brew uninstall`'d,
  # so the nix copy isn't shadowed on PATH (see brew-to-nix migration rule).
  home.packages = with pkgs; [
    # dev core
    git coreutils gnutar fd ripgrep silver-searcher tree tree-sitter
    editorconfig-core-c htop openssl pandoc p7zip
    # shell tooling
    shellcheck shfmt
    # NOTE: zimfw (zsh framework) is NOT listed here — it ships only zimfw.zsh
    # (no binary), pulled into the closure by the ZIM_FW_INIT fragment below.
    # Migrated off brew → nixpkgs on both OSes (issue 05a); the passthrough zshrc
    # no longer sources it from /opt/homebrew (mac) or linuxbrew (NixOS, absent).
    # clojure (Doom clojure module); guile dropped
    clojure clojure-lsp leiningen
    # misc
    lua ffmpeg gifsicle html-tidy
    # for fun
    nethack
    # NOTE: zork has no nixpkgs package — stays a brew formula (4e homebrew module).

    # terminal (binary; config is passthrough alacritty/). Cross-platform via nix.
    alacritty

    # fonts (issue 04e) — cross-platform via nix (macOS ~/Library/Fonts, NixOS
    # fontconfig). Nerd Fonts for editor/terminal glyphs + web-dev typefaces.
    nerd-fonts.fira-code
    nerd-fonts.hack
    fira-code fira-sans fira-mono
    source-code-pro source-sans-pro source-serif-pro
    inconsolata roboto lato lora merriweather vt323
    # Exact families the Doom config selects (doom/config.el): zf/fixed-font
    # "Hack" and zf/variable-font "Merriweather Sans". These differ from the
    # nerd-fonts.hack family ("Hack Nerd Font") and merriweather ("Merriweather"),
    # so they need their own packages. Missing on a clean box → GUI Emacs aborts
    # font setup mid-init (white theme, dead leader); hestia only worked via stray
    # macOS fonts. (issue 05d)
    hack-font
    merriweather-sans
  ]
  # Linux-only Doom/Emacs deps (issue 05d). macOS satisfies these natively
  # (system python; no X11 tools checked), so they're scoped to NixOS to keep
  # hestia untouched.
  #   - emacs30-pgtk: pure-GTK Emacs, right under Wayland/GNOME (mac keeps the
  #     emacs-plus@30 brew, issue 01). Doom itself stays hand-managed on both.
  #   - python3: `:lang python` module hard dep (doom doctor error).
  #   - xclip/xdotool/xwininfo: X11 clipboard/window tools a Doom module needs
  #     (doom doctor error). NOTE: these are X11; under Wayland they work only
  #     for XWayland. True Wayland clipboard would also want wl-clipboard —
  #     deferred (revisit with the Wayland/ricing work, #08).
  ++ lib.optionals pkgs.stdenv.isLinux [
    emacs30-pgtk
    python3
    xclip xdotool xorg.xwininfo
  ];

  # Make HM-installed fonts (nerd-fonts + typefaces above) discoverable by apps.
  # Required on NixOS; macOS font handling is native, so scope to Linux. (05c)
  fonts.fontconfig.enable = pkgs.stdenv.isLinux;

  # --- per-project dev environments (issue 04d; replaces mise) ---
  # nix-direnv + a project `.envrc` containing `use flake` build a pinned,
  # reproducible toolchain from that project's flake devShell and load it on cd.
  # This supersedes mise (runtime version manager) — chosen for reproducibility
  # and cross-machine parity over mise's looser version strings. See ADR (04d).
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # --- neovim (backup editor; Emacs/Doom is primary, see issue 04b) ---
  # Native module (config-as-nix) by choice. Mirrors the Doom workflow's TOOLS
  # rather than its config: evil is native to (neo)vim, SPC leader, nord theme,
  # telescope≈vertico, nvim-tree≈treemacs, cmp≈corfu, which-key, treesitter.
  # Deliberately light — a capable fallback, not a second IDE.
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    defaultEditor = false; # EDITOR stays vim per zshenv; this is a backup

    plugins = with pkgs.vimPlugins; [
      # NOTE: proper theme (nord, matching Doom) deferred to issue 08 (Flavours).
      # Using a built-in colorscheme for now so a theme-load failure can't abort
      # the rest of the config (which is what broke leader maps the first time).
      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = "require('nvim-treesitter.configs').setup({ highlight = { enable = true }, indent = { enable = true } })";
      }
      { plugin = telescope-nvim; type = "lua";
        config = ''
          local t = require('telescope.builtin')
          vim.keymap.set('n', '<leader>ff', t.find_files, { desc = 'Find files' })
          vim.keymap.set('n', '<leader>fg', t.live_grep,  { desc = 'Grep' })
          vim.keymap.set('n', '<leader>fb', t.buffers,    { desc = 'Buffers' })
        '';
      }
      { plugin = nvim-tree-lua; type = "lua";
        config = ''
          require('nvim-tree').setup()
          vim.keymap.set('n', '<leader>op', '<cmd>NvimTreeToggle<cr>', { desc = 'Project tree' })
        '';
      }
      { plugin = lualine-nvim; type = "lua"; config = "require('lualine').setup({ options = { theme = 'auto' } })"; }
      { plugin = which-key-nvim; type = "lua"; config = "require('which-key').setup()"; }
      { plugin = todo-comments-nvim; type = "lua"; config = "require('todo-comments').setup()"; }
      { plugin = gitsigns-nvim; type = "lua"; config = "require('gitsigns').setup()"; }
      # completion (cmp ≈ corfu)
      cmp-nvim-lsp cmp-buffer cmp-path luasnip cmp_luasnip
      { plugin = nvim-cmp; type = "lua";
        config = ''
          local cmp = require('cmp')
          cmp.setup({
            snippet = { expand = function(a) require('luasnip').lsp_expand(a.body) end },
            mapping = cmp.mapping.preset.insert({
              ['<Tab>'] = cmp.mapping.select_next_item(),
              ['<S-Tab>'] = cmp.mapping.select_prev_item(),
              ['<CR>'] = cmp.mapping.confirm({ select = true }),
            }),
            sources = { { name = 'nvim_lsp' }, { name = 'luasnip' }, { name = 'buffer' }, { name = 'path' } },
          })
        '';
      }
    ];

    extraLuaConfig = ''
      -- leader = SPC, matching Doom
      vim.g.mapleader = ' '
      vim.g.maplocalleader = ' '

      local o = vim.opt
      o.number = true
      o.relativenumber = true
      o.shiftwidth = 2          -- matches Doom evil-shift-width
      o.tabstop = 2
      o.expandtab = true
      o.smartindent = true
      o.ignorecase = true
      o.smartcase = true
      o.termguicolors = true
      o.clipboard = 'unnamedplus'
      o.undofile = true         -- persistent undo (writable state dir, unlike ~/.vim)
      o.signcolumn = 'yes'
      o.scrolloff = 5

      -- built-in theme for now (issue 08 swaps in a real one via Flavours).
      -- pcall so a bad colorscheme never aborts the config below it.
      pcall(vim.cmd.colorscheme, 'habamax')

      -- a couple of Doom-ish leader maps
      vim.keymap.set('n', '<leader>bd', '<cmd>bdelete<cr>', { desc = 'Kill buffer' })
      vim.keymap.set('n', '<leader>w',  '<C-w>',            { desc = 'Window' })
    '';
  };

  # --- shell (zsh + zim) ---
  # zshrc is passthrough (verbatim), so it can't interpolate a nix store path.
  # HM writes the zimfw framework path into this fragment; zshrc sources it and
  # then `source "$ZIM_FW_INIT" init`. One path-free line, identical on both OSes
  # (replaces the old uname Darwin/Linux brew/linuxbrew case). See issue 05a.
  # zimfw is used by sourcing zimfw.zsh with an action ("init", "upgrade", …);
  # there is no `zimfw` binary. Export the path and define the CLI as a function
  # so `zimfw upgrade` etc. work off the nix store path (brew provided this
  # function before; nixpkgs does not).
  xdg.configFile."zsh/zim-fw-init.zsh".text = ''
    export ZIM_FW_INIT="${pkgs.zimfw}/zimfw.zsh"
    zimfw() { source "$ZIM_FW_INIT" "$@"; }
  '';

  home.file.".zshenv".source = ./zshenv;
  home.file.".zprofile".source = ./zprofile;
  home.file.".zshrc".source = ./zshrc;
  home.file.".zimrc".source = ./zimrc;
  home.file.".aliases".source = ./aliases;

  # --- git ---
  home.file.".gitconfig".source = ./gitconfig;
  home.file.".gitignore_global".source = ./gitignore_global;

  # --- ssh (native module; config declarative, keys stay manual/out-of-flake) ---
  # Generates ~/.ssh/config. Key generation + the keys themselves are NOT managed
  # here (secrets don't belong in the flake). UseKeychain is macOS-only — guard it
  # when the NixOS host is added (issue 05).
  programs.ssh = {
    enable = true;
    # HM 25.11 deprecated the top-level `Host *` options (addKeysToAgent etc) in
    # favour of an explicit matchBlocks."*"; opt out of the built-in defaults and
    # define our own so nothing is silently dropped when they're removed. (05c;
    # supersedes the 25.05 workaround from 4c.)
    enableDefaultConfig = false;
    matchBlocks."*" = {
      addKeysToAgent = "yes";
      identityFile = "~/.ssh/id_ed25519";
      # UseKeychain is macOS-only (Apple keychain) — guard so athena (NixOS)
      # doesn't get an invalid option.
      extraOptions = lib.optionalAttrs pkgs.stdenv.isDarwin { UseKeychain = "yes"; };
    };
  };

  # --- terminal multiplexer ---
  home.file.".tmux.conf".source = ./tmux.conf;

  # vim dropped (issue 04b): neovim is the backup editor, and its viAlias/vimAlias
  # make `vi`/`vim` launch nvim. Old vimrc/.vim removed from the repo.

  # --- alacritty (XDG) ---
  xdg.configFile."alacritty".source = ./alacritty;

  # --- hammerspoon (macOS-only app; NixOS window mgmt → future hyprland issue) ---
  home.file.".hammerspoon" =
    lib.mkIf pkgs.stdenv.isDarwin { source = ./hammerspoon; };

  # --- doom: emacs install symlink + private config (both out-of-store) ---
  # ~/.emacs.d → the hand-cloned doomemacs install. Doom itself stays imperative
  # (NOT nix-managed; ADR-0002): you `git clone` doomemacs into ~/code and run
  # `doom install`/`doom sync`/`doom env` yourself (see docs/new-host.md). This is
  # only the declarative symlink — it dangles until the clone exists, which is
  # fine since cloning is a bootstrap step.
  home.file.".emacs.d".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/doomemacs";

  # ~/.config/doom → private config in this repo, out-of-store so the Emacs GUI
  # (no DOOMDIR) and CLI (DOOMDIR) never diverge between rebuilds (issue 01).
  xdg.configFile."doom".source =
    config.lib.file.mkOutOfStoreSymlink "${dotfiles}/doom";
}
