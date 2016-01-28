" =================================================================
" vimrc
"
" author: zachfedor
" date: 2016.01.23
" =================================================================


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
let mapleader="\<space>"           " map <space> to leader
                            " mapping to escape insert
inoremap jk <ESC>
                            " mapping to copy to system clipboard
vmap <leader>y "+y


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
" plugins
" -----------------------------------------------------------------
call plug#begin('~/.vim/plugged')

" general -----------
Plug 'ctrlpvim/ctrlp.vim'
Plug 'reedes/vim-wheel'
Plug 'rking/ag.vim'
Plug 'tpope/vim-sensible'

" style -----------
Plug 'chriskempson/base16-vim'
Plug 'reedes/vim-colors-pencil'
Plug 'reedes/vim-thematic'
Plug 'vim-airline/vim-airline'

" development -----------
" Plug 'mattn/emmet-vim'
Plug 'raimondi/delimitmate'
Plug 'scrooloose/syntastic'
Plug 'shougo/neocomplete.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
" Plug 'valloric/youcompleteme'

" tools -----------
Plug 'airblade/vim-gitgutter'
Plug 'godlygeek/tabular'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'xuyuanp/nerdtree-git-plugin'

" languages -----------
Plug 'hail2u/vim-css3-syntax'
Plug 'marijnh/tern_for_vim'
Plug 'pangloss/vim-javascript'
Plug 'plasticboy/vim-markdown'  " needs: godlygeek/tabular
Plug 'shutnik/jshint2.vim'

" writing -----------
Plug 'reedes/vim-pencil'
Plug 'reedes/vim-wordy'

call plug#end()


" -----------------------------------------------------------------
" plugin config
" -----------------------------------------------------------------
" jshint2.vim -----------
let jshint2_command = '/usr/local/bin/jshint'   " path to jshint
let jshint2_read    = 1                 " run jshint on file read
let jshint2_save    = 1                 " run jshint on file save

" neocomplete -----------
let g:acp_enableAtStartup           = 0 " disable autocomplete at start
let g:neocomplete#enable_at_startup = 0 " disable neocomplete at start
let g:neocomplete#enable_smart_case = 1 " ignore case unless specified

if exists(":NeoCompleteToggle")
                                        " mapping to toggle autocomplete
    nmap <leader>ac :NeoCompleteToggle
endif

" syntastic -----------                 " recommended settings
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list            = 1
let g:syntastic_check_on_open            = 1
let g:syntastic_check_on_wq              = 0

" tabular -----------
if exists(":Tabularize")
                                        " mapping to align by =
    nmap <leader>te :Tabularize /=<CR>
    vmap <leader>te :Tabularize /=<CR>
                                        " mapping to align by :
    nmap <leader>tc :Tabularize /:<CR>
    vmap <leader>tc :Tabularize /:<CR>
                                        " mapping to align by |
    nmap <leader>tt :Tabularize /\|<CR>
    vmap <leader>tt :Tabularize /\|<CR>
endif

" vim-gitgutter -----------
if exists(":GitGutterToggle")
                                        " mapping gutter toggle
    nmap <leader>gg :GitGutterToggle<CR>
endif

" vim-markdown -----------
let g:vim_markdown_folding_level = 4      " folds headings 4 levels deep
let g:vim_markdown_frontmatter   = 1      " highlight jekyll frontmatter

" vim-wiki -----------
let wiki             = {}               " create vimwiki object
let wiki.path        = '~/Dropbox/wiki/'
let wiki.syntax      = 'markdown'
let wiki.ext         = '.md'
let blog             = {}               " create vimwiki object
let blog.path        = '~/Dropbox/wiki/blog/'
let blog.path_html   = '~/Dropbox/wiki/html/'
let blog.auto_export = 1

let g:vimwiki_list   = [wiki, blog]     " init wiki objects
