" =================================================================
" vimrc
"
" author: zachfedor
" date: 2016.01.23
" =================================================================


" -----------------------------------------------------------------
" plugins
" -----------------------------------------------------------------
call plug#begin('~/.vim/plugged')

" general -----------
Plug 'tpope/vim-sensible'
Plug 'reedes/vim-wheel'
Plug 'ctrlpvim/ctrlp.vim'

" style -----------
Plug 'reedes/vim-thematic'
Plug 'vim-airline/vim-airline'
Plug 'chriskempson/base16-vim'
Plug 'reedes/vim-colors-pencil'

" development -----------
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
" Plug 'mattn/emmet-vim'

" tools -----------
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdtree'
Plug 'xuyuanp/nerdtree-git-plugin'

" writing -----------
Plug 'reedes/vim-pencil'
Plug 'reedes/vim-wordy'

call plug#end()


" -----------------------------------------------------------------
" general
" -----------------------------------------------------------------
set nocompatible            " always vim, never vi
set scrolloff=8             " line buffer to scroll off screen
set ignorecase              " ignore case in search
set smartcase               " uppercase search pattern override

if has("mouse")
    set mouse=a             " allow mouse in all modes
endif

if has("vms")
    set nobackup            " use versions if they exist
else
    set backup              " if not, use backups
endif
set backupdir=~/.vim-tmp    " centralize all backup files
set directory=~/.vim-tmp    " centralize all swap files

if has("autocmd")           " remember last cursor position
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |
        \ exe "normal! g'\"" |
        \ endif
endif


" -----------------------------------------------------------------
" editor settings
" -----------------------------------------------------------------
set softtabstop=4           " indent width for editing
set shiftwidth=4            " indent width for autoindent
set expandtab               " convert tabs to spaces


" -----------------------------------------------------------------
" keybindings
" -----------------------------------------------------------------
let mapleader=" "           " map <space> to leader

inoremap jk <ESC>           " exit insert mode with jk


" -----------------------------------------------------------------
" style
" -----------------------------------------------------------------
let g:thematic#themes = {
\ 'bespin'  : { 'typeface': 'Menlo',
\               'font-size': 16,
\               'background': 'dark',
\               'airline-theme': 'badwolf',
\               }
\ }


" -----------------------------------------------------------------
" language
" -----------------------------------------------------------------
" markdown -----------
if has("autocmd")
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
endif


" -----------------------------------------------------------------
" plugin config
" -----------------------------------------------------------------
" vim-wiki -----------
let wiki = {}
let wiki.path = '~/Dropbox/wiki/'
let wiki.syntax = 'markdown'
let wiki.ext = '.md'
let blog = {}
let blog.path = '~/Dropbox/wiki/blog/'
let blog.path_html = '~/Dropbox/wiki/html/'
let blog.auto_export = 1

let g:vimwiki_list = [wiki, blog]   " create wikis from object
