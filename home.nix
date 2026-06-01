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

{ config, pkgs, ... }:

let
  dotfiles = "/Users/zach/.dotfiles";
in {
  home.stateVersion = "25.05";

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
    # These top-level options ARE the default `Host *` block in this HM version.
    # Setting them here (rather than a separate matchBlocks."*") avoids emitting
    # two conflicting `Host *` stanzas.
    addKeysToAgent = "yes";
    # UseKeychain (macOS-only) + the identity key have no dedicated options, so
    # go through extraConfig. Guard UseKeychain when the NixOS host lands (#05).
    extraConfig = ''
      IdentityFile ~/.ssh/id_ed25519
      UseKeychain yes
    '';
  };

  # --- terminal multiplexer ---
  home.file.".tmux.conf".source = ./tmux.conf;

  # vim dropped (issue 04b): neovim is the backup editor, and its viAlias/vimAlias
  # make `vi`/`vim` launch nvim. Old vimrc/.vim removed from the repo.

  # --- alacritty (XDG) ---
  xdg.configFile."alacritty".source = ./alacritty;

  # --- hammerspoon ---
  home.file.".hammerspoon".source = ./hammerspoon;

  # --- doom private config (out-of-store; see header note) ---
  xdg.configFile."doom".source =
    config.lib.file.mkOutOfStoreSymlink "${dotfiles}/doom";
}
