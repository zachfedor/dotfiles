" =================================================================
" vimrc
"
" author: zachfedor
" date: 2016.01.23
" =================================================================


" -----------------------------------------------------------------
" general
" -----------------------------------------------------------------
                        " map <space> to leader
let mapleader=" "
                        " always vim, never vi
set nocompatible
                        " line buffer to scroll off screen
set scrolloff=8
                        " allow hidden buffers
set hidden
                        " ignore case in search
set ignorecase
                        " uppercase search pattern override
set smartcase
                        " natural split opening sides
set splitbelow
set splitright

if has("mouse")
                        " allow mouse in all modes
    set mouse=a
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
                            " show current line number with relative numbers
set number
set relativenumber


" -----------------------------------------------------------------
" keybindings
" -----------------------------------------------------------------
                            " mapping to escape insert
inoremap jk <ESC>
                            " mapping to copy to system clipboard
vmap <leader>y "+y
                            " mappings to navigate buffers
nmap <leader>bn :bn<CR>
nnoremap K :bn<CR>
nmap <leader>bp :bp<CR>
nnoremap J :bp<CR>
                            " mapping to close buffer
nmap <leader>bd :bd<CR>
                            " mapping to navigate splits
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>


" -----------------------------------------------------------------
" language
" -----------------------------------------------------------------
augroup LanguageSupport
    autocmd!
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
    " autocmd BufNewFile,BufReadPost *.hbs set filetype=html
augroup END


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
" Plug 'reedes/vim-thematic'
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
Plug 'mustache/vim-mustache-handlebars'
Plug 'pangloss/vim-javascript'
Plug 'plasticboy/vim-markdown'  " needs: godlygeek/tabular
Plug 'shutnik/jshint2.vim'

" writing -----------
Plug 'reedes/vim-pencil'
Plug 'reedes/vim-wordy'
" Plug 'vimwiki/vimwiki'
Plug '~/code/vimwiki'

call plug#end()


" -----------------------------------------------------------------
" plugin config
" -----------------------------------------------------------------
" base16-vim -----------
let base16colorspace=256

" ctrlp -----------
function CtrlPSettings()
if exists(":CtrlP")
                                    " use nearest .git/ as current project
    let g:ctrlp_working_path_mode = 'r'
                                    " ignore files in .gitignore
    let g:ctrlp_user_command      = ['.git', 'cd %s && git ls-files']
                                    " mapping for fuzzy find
    nmap <leader>ff :CtrlP<CR>
    nmap <leader>bf :CtrlPBuffer<CR>
endif
endfunction

" jshint2.vim -----------
let jshint2_command = '/usr/local/bin/jshint'   " path to jshint
let jshint2_read    = 1                 " run jshint on file read
let jshint2_save    = 1                 " run jshint on file save

" neocomplete -----------
function NeoCompleteSettings()
if exists(":NeoCompleteToggle")
    let g:acp_enableAtStartup           = 0 " disable autocomplete at start
    let g:neocomplete#enable_at_startup = 0 " disable neocomplete at start
    let g:neocomplete#enable_smart_case = 1 " ignore case unless specified
                                        " mapping to toggle autocomplete
    nmap <leader>ac :NeoCompleteToggle
endif
endfunction

" nerdtree -----------
function NerdTreeSettings()
if exists(":NERDTree")
    nmap <leader>ft :NERDTree<CR>
endif
endfunction

" syntastic -----------
function SyntasticSettings()
if exists(":SyntasticCheck")
                                        " recommended settings
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list            = 1
    let g:syntastic_check_on_open            = 0
    let g:syntastic_check_on_wq              = 0
                                        " use html5 version
    let g:syntastic_html_tidy_exec           = '/usr/bin/tidy'
                                        " mapping to turn off error buffer
    nmap <leader>sr :SyntasticReset<CR>
endif
endfunction


" tabular -----------
function TabularSettings()
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
endfunction

" thematic -----------
" let g:thematic#defaults = {
"             \ 'typeface': 'Menlo',
"             \ 'font-size': 16,
"             \ }
" let g:thematic#themes = {
" \ 'bespin'  : { 'background': 'dark',
" \               'colorscheme': 'base16-bespin',
" \               'airline-theme': 'badwolf'
" \               },
" \ 'ocean'   : { 'colorscheme': 'base16-ocean',
" \               'background': 'dark'
" \               }
" \ }
" let g:thematic#theme_name = 'bespin'

" vim-airline  -----------
let g:airline_left_sep                   = " "  " use space to create squared sections
let g:airline_right_sep                  = " "
let g:airline#extensions#tabline#enabled = 1    " enable the tab bar at top
" let g:airline#extensions#tabline#fnamemod = ':t'

" vim-gitgutter -----------
function VimGitGutterSettings()
if exists(":GitGutterToggle")
                                        " mapping gutter toggle
    nmap <leader>gg :GitGutterToggle<CR>
    let g:gitgutter_override_sign_column_highlight = 0
endif
endfunction

" vim-markdown -----------
let g:vim_markdown_folding_level = 1      " folds headings 4 levels deep
let g:vim_markdown_frontmatter   = 1      " highlight jekyll frontmatter

" vim-wiki -----------
let wiki             = {}               " create general wiki
let wiki.path        = '~/Dropbox/wiki/'
let wiki.syntax      = 'markdown'
let wiki.ext         = '.md'
let blog             = {}               " create work wiki
let blog.path        = '~/sites/zachfedor.github.io/'
let blog.syntax      = 'markdown'
let blog.ext         = '.md'

let g:vimwiki_list   = [wiki, blog]     " init wiki objects

" settings functions -----------
augroup PluginSettings
    autocmd!
    autocmd VimEnter * :call CtrlPSettings()
    autocmd VimEnter * :call NerdTreeSettings()
    autocmd VimEnter * :call NeoCompleteSettings()
    autocmd VimEnter * :call TabularSettings()
    autocmd VimEnter * :call SyntasticSettings()
    autocmd VimEnter * :call VimGitGutterSettings()
augroup END


" -----------------------------------------------------------------
" style
" -----------------------------------------------------------------
set background=dark
colorscheme base16-eighties
                            " set colors of line number column
                            " as fix for thematic plugin
" augroup mystylesforvim
"     autocmd!
"     autocmd BufEnter * highlight LineNr ctermbg=00
"     autocmd BufEnter * highlight LineNr ctermfg=18
"     autocmd BufEnter * highlight SignColumn ctermbg=00
"     autocmd BufEnter * highlight GitGutterAdd ctermbg=00
"     autocmd BufEnter * highlight GitGutterChange ctermbg=00
"     autocmd BufEnter * highlight GitGutterDelete ctermbg=00
"     autocmd BufEnter * highlight GitGutterChangeDelete ctermbg=00
" augroup END

                            " set colors of line number column
highlight LineNr ctermbg=00
" highlight LineNr ctermfg=18
                            " set colors of gitgutter column
highlight SignColumn ctermbg=00
highlight GitGutterAdd ctermbg=00
highlight GitGutterChange ctermbg=00
highlight GitGutterDelete ctermbg=00
highlight GitGutterChangeDelete ctermbg=00
