""""""""""""""""
" VIM SETTINGS "
""""""""""""""""

" use vim settings, not vi
set nocompatible

" use versions or backup
if has("vms")
	set nobackup
else
	set backup
endif
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp	" temp files in central spot
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp	" temp files in central spot

" key bindings
map Q gq				" don't use EX mode, use Q for formatting
inoremap jk <ESC>			" jk escapes from INSERT
inoremap <C-U> <C-G>u<C-U>		" Ctrl-G u first breaks undo so you can undo Ctrl-U after inserting linebreak
let mapleader = " "			" set space to leader key
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>			" sets viewport scroll to 3 lines at a time

" initiate pathogen
call pathogen#infect()
call pathogen#helptags()

" other settings
set backspace=indent,eol,start		" allow backspace over everything in INSERT
syntax enable				" syntax highlighting
set visualbell
set ruler				" shows count in bottom-right
"set number				" shows line numbers
set tabstop=4				" sets tab size to 4
set expandtab				" tab key inserts spaces
set shiftwidth=4			" sets spaces used for autoindent
set history=500				" keep last 500 commands
runtime macros/matchit.vim		" extended matching with %
set wildmenu				" tab completion
set wildmode=list:longest		" bash-like completion
set showcmd				" shows incomplete commands in bottom-right
set incsearch				" highlights search terms incrementally
set ignorecase				" / searches are case sensitive only with capital letter in search expression
set smartcase				" * searches stay case sensitive
set title				" sets title in terminal
set scrolloff=8				" buffers 3 extra lines when scrolling off the page
if has('mouse')
	set mouse=a			" allows mouse
endif

if has("autocmd")
	filetype plugin indent on	"enable file-type detection and default indentation
	augroup vimrcEx
	au!
	autocmd FileType text setlocal textwidth=78	" set width for text files
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
	autocmd BufReadPost *		" return to last known cursor position
		\ if line("'\"") > 1 && line("'\"") <= line("$") |
		\	exe "normal! g`\"" |
		\ endif
	augroup END
else
	set autoindent
    filetype plugin indent on
endif " has("autocmd")


" color options
set background=dark
colorscheme solarized
syntax on
if &t_Co > 2 || has("gui_running")
	syntax on
	set hlsearch
endif

" settings for vimwiki
let wiki = {}
let wiki.path = '~/Dropbox/wiki/'
let wiki.syntax = 'markdown'
let wiki.ext = '.md'

let blog = {}
let blog.path = '~/Dropbox/wiki/blog/'
let blog.path_html = '~/Dropbox/wiki/html/'
let blog.auto_export = 1

let g:vimwiki_list = [wiki, blog]

