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
                        " add autocomplete with spell check
set complete+=kspell
                        " point to a spellfile symlinked to dotfiles
set spellfile=$HOME/.vim/spell-en.utf-8.add

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
                            " mappings to scroll faster
nnoremap K 10k
nnoremap J 10j
                            " mappings to navigate buffers
nmap <leader>bn :bn<CR>
nmap <leader>bp :bp<CR>
                            " mapping to close buffer
nmap <leader>bd :bd<CR>
                            " mapping to navigate splits
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
                            " mapping to create timestamps
nnoremap <leader>dd "=strftime("%Y-%m-%d")<CR>P
inoremap <C-d><C-d> <C-R>=strftime("%Y-%m-%d")<CR>
nnoremap <leader>dt "=strftime("%H:%M:%S")<CR>P
inoremap <C-d><C-t> <C-R>=strftime("%H:%M:%S")<CR>
nnoremap <leader>ds "=strftime("%Y-%m-%d %H:%M:%S")<CR>P
inoremap <C-d><C-s> <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>


" -----------------------------------------------------------------
" language
" -----------------------------------------------------------------
let g:html_indent_inctags = "html,body,head,tbody"

augroup LanguageSupport
    autocmd!
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
    autocmd FileType markdown setlocal spell
    autocmd FileType gitcommit setlocal spell
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
Plug 'editorconfig/editorconfig-vim'
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
Plug 'tpope/vim-speeddating'
Plug 'xuyuanp/nerdtree-git-plugin'

" languages -----------
Plug 'marijnh/tern_for_vim'
" Javascript
Plug 'elzr/vim-json'
Plug 'gavocanov/vim-js-indent'
Plug 'mxw/vim-jsx'
Plug 'othree/yajs.vim'
" CSS
Plug 'hail2u/vim-css3-syntax'
" Other
Plug 'jamshedvesuna/vim-markdown-preview'
Plug 'mustache/vim-mustache-handlebars'
Plug 'plasticboy/vim-markdown'  " needs: godlygeek/tabular

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

" editorconfig -----------
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
let g:EditorConfig_exec_path = '/usr/local/bin/editorconfig'

" emmet-vim -----------
" let g:user_emmet_leader_key = '<C-m>'
" let g:user_emmet_expandabbr_key = '<C-m>m'
" let g:user_emmet_balancetaginward_key = '<C-m>a'
" let g:user_emmet_balancetagoutward_key = '<C-m>i'
" let g:user_emmet_next_key = '<C-m>n'
" let g:user_emmet_prev_key = '<C-m>p'

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
                                        " use eslint instead of jshint
    let g:syntastic_javascript_checkers = ['eslint']
                                        " mapping to turn off error buffer
    nmap <leader>ss :SyntasticCheck<CR>
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
" let g:airline#extensions#tabline#enabled = 1    " enable the tab bar at top
" let g:airline#extensions#tabline#fnamemod = ':t'

" vim-gitgutter -----------
function VimGitGutterSettings()
if exists(":GitGutterToggle")
                                        " mapping gutter toggle
    nmap <leader>gg :GitGutterToggle<CR>
    let g:gitgutter_override_sign_column_highlight = 0
endif
endfunction

" vim-jsx -----------
let g:jsx_ext_required = 0              " allow JSX in normal .js files

" vim-markdown -----------
let g:vim_markdown_folding_level = 1    " folds headings 4 levels deep
let g:vim_markdown_frontmatter   = 1    " highlight jekyll frontmatter

" vim-markdown-preview -----------
let vim_markdown_preview_hotkey='<space>m'    " changes default hotkey of <C-p>

" vim-wiki -----------
let wiki             = {}               " create general wiki
let wiki.path        = '~/Dropbox/wiki/'
let wiki.syntax      = 'markdown'
let wiki.ext         = '.md'
let work             = {}               " create work wiki
let work.path        = '~/Dropbox/wiki/work'
let work.syntax      = 'markdown'
let work.ext         = '.md'
let blog             = {}               " create blog wiki
let blog.path        = '~/sites/zachfedor.github.io/'
let blog.syntax      = 'markdown'
let blog.ext         = '.md'

let g:vimwiki_list   = [wiki, work, blog]   " init wiki objects
let g:vimwiki_hl_headers = 1
let g:vimwiki_hl_cb_checked = 1
let g:vimwiki_listsyms = ' ...x'
                            " mapping to open common notes
nnoremap <leader>wf <Plug>VimwikiUISelect
nnoremap <leader>wr :find ~/wiki/review/current.md<CR>
nnoremap <leader>ws :find ~/wiki/work/current.md<CR>
nnoremap <leader>wb :find ~/sites/zachfedor.github.io/index.md<CR>
nnoremap <leader>w<leader>r <Plug>VimwikiRenameLink
nnoremap <leader>w= <Plug>VimwikiAddHeaderLevel
nnoremap <leader>w- <Plug>VimwikiRemoveHeaderLevel

" vim-wheel -----------
                            " " ∆ is <Alt-j>
" let g:wheel#map#up   = '<A-j>'
                            " " ˚ is <Alt-k>
" let g:wheel#map#down = '<A-k>'

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
colorscheme base16-twilight
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


" -----------------------------------------------------------------
" functions
" -----------------------------------------------------------------
" markdown header levels
function!EditMarkdownHeader(dir)
    " get current line
    let line = getline('.')
    if strlen(line) > 0
        " split into array of words
        let linearray = split(line)
        " split first word into array of characters
        let pre = split(linearray[0], '\zs')

        if a:dir
            " increase header
            if index(pre, '#') > -1
                " line is already a header
                let line = '#' . line
            else
                " line is not a header
                let line = '# ' . line
            endif
        else
            " decrease header
            if index(pre, '#') > -1
                " line is a header
                if linearray[0] ==# '#'
                    " line is an h1
                    " remove header entirely
                    let line = join(linearray[1:])
                else
                    " line is h2 or higher
                    " remove just one '#'
                    let line = join(pre[1:], '') . ' ' . join(linearray[1:])
                endif
            endif
            " else line is not a header, do nothing
        endif

        " replace current line with altered line
        call setline('.', line)
    endif
endfunction
    " mapping to edit markdown header levels
nnoremap = :call EditMarkdownHeader(1)<CR>
nnoremap - :call EditMarkdownHeader(0)<CR>


" preview markdown -----------
function!PreviewMarkdown()
    " src: https://gist.github.com/natesilva/960015
    " let MARKDOWN_CMD = 'grip'
    let MARKDOWN_CMD = 'markdown'
    let BROWSER_CMD = 'open'

    silent update
    let output_name = tempname() . '.html'

    let original_encoding = &fileencoding
    let original_bomb = &bomb
    if original_encoding != 'utf-8' || original_bomb == 1
        set nobomb
        set fileencoding=utf-8
        silent update
    endif

    let file_header = ["<!DOCTYPE html>", '<html>', '<head>',
        \ '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">',
        \ '<title>Markdown Preview</title>',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssreset/reset-min.css">',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssbase/base-min.css">',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssfonts/fonts-min.css">',
        \ '<style>body{padding:20px;}div#container{background-color:#F2F2F2;padding:0 20px;margin:0px;border:solid #D0D0D0 1px;}</style>',
        \ '</head>', '<body>', '<div id="container">']
    call writefile(file_header, output_name)

    let markdown_cmd = '!' . MARKDOWN_CMD . ' "' . expand('%:p') . '" >> "' .
        \ output_name . '"'
    silent exec markdown_cmd

    silent exec '!echo "</div></body></html>" >> "' .
        \ output_name . '"'


    if original_encoding != 'utf-8' || original_bomb == 1
        if original_bomb == 1
            set bomb
        endif
        silent exec 'set fileencoding=' . original_encoding
        silent update
    endif

    silent exec '!' . BROWSER_CMD . ' "' . output_name . '"'

    exec input('Press ENTER to continue...')
    echo
    exec delete(output_name)
endfunction

" map <leader>mp :call PreviewMarkdown()<CR>
