set nocompatible      " VI improve allowed
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'VundleVim/Vundle.vim'

""""""""""""""""""" All plugins here """""""""""""""""""

" Syntax highlighting for perl-mason
Plugin 'aming/vim-mason'

" Nerd tree directory navigator
Plugin 'scrooloose/nerdtree'

" NERDTree git plugin
Plugin 'Xuyuanp/nerdtree-git-plugin'

" Undo tree plugin
Plugin 'mbbill/undotree'

" Allow for surrounding notation
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'

" Haskell folding capability
Plugin 'vim-scripts/haskellFold'

" Git
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'

" Differenc color parentheses
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'luochen1990/rainbow'

" Indentation guide
Plugin 'nathanaelkane/vim-indent-guides'

" Fuzzy File Finder
Plugin 'kien/ctrlp.vim'

" JS React highlighting
" Plugin 'pangloss/vim-javascript'
" Plugin 'maxmellon/vim-jsx-pretty'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'

""""""""""""""""""" Plugins end """""""""""""""""""

call vundle#end()            " All plugins before here
filetype plugin indent on    " turn filetypes back on
filetype on

" Nerd tree options
" Use CTRL-n to open NERDtree
map <C-n> :NERDTreeToggle<CR>
" Undo tree options
" Use leader z to open undo
map <leader>z :UndotreeToggle<CR>
:let g:NERDTreeWinSize=60

" Open NERDTree automatically when vim starts up on opening a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

" Git plugin settings
set updatetime=100
nnoremap <Leader>gb :Gblame<CR>

" Rainbow parentheses
map <leader>r :RainbowParenthesesToggleAll<CR>

" Indentation guide
map <leader>i :IndentGuidesToggle<CR>

" Fuzzy finding files
cmap ep CtrlP

" Colors
colorscheme desert
syntax enable          " Enable syntax processing
set colorcolumn=100    " Hilights the 80th column
" Sets column color to grey
highlight ColorColumn ctermbg=8

" Tabs
set smarttab           " Indents to the correct spot first time
set shiftwidth=4       " Code defaults to 4 space indents
set tabstop=4          " Number of visual spaces per TAB
set softtabstop=4      " Number of spaces in a tab when editing
set autoindent         " Automatically indents when enter is pressed
set expandtab          " Makes all tabs into spaces

" UI
set relativenumber     " Show line numbers relative to cursor
set number             " Shows the line number of the current line.
set showcmd            " Shows the most recent command
set cursorline         " Highlight current line
filetype indent on     " Load filetype-specific indent files
set wildmenu           " Visual autocomplete for command menue
set lazyredraw         " Redraw only when needed
set showmatch          " Highlight matching [{()}]
set ruler

" Searching
set incsearch          " Search as characters are entered
set hlsearch           " Highlight search matches
set dictionary=/usr/share/dict/words "dictionary for searching

" Folding
set foldenable         " Enables code folding
set foldnestmax=10     " Prevents too many folds
nnoremap <space> za
                       " Set space to close folds
set foldmethod=indent  " Default to using indentation for folds
set foldlevelstart=20  " Start with all folds open

" Note, perl automatically sets foldmethod in the syntax file
autocmd Syntax c,cpp,vim,xml,html,xhtml setlocal foldmethod=syntax
autocmd Syntax c,cpp,vim,xml,html,xhtml,perl normal zR

" Remaps
nnoremap gV `[v`]
                       " Highlight last inserted text
noremap <C-i> ^
                       " Go to beginning of line
noremap <C-a> $
                       " Go to end of line
noremap <C-j> kddpkJ
                       " Joins the previous line to the end of the current line (J in the oppisite direction)

" Insert mode moveing
inoremap <C-h> <Right>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>
inoremap jk <esc>
                       " jk kes act as esc together

set backspace=indent,eol,start
set redrawtime=10000

" Meta changes
silent execute '!mkdir "~/.swap"'
set backupdir=~/.swap//,.,/tmp//
set directory=~/.swap//,.,/tmp//

" File Specific changes
autocmd Filetype javascript.jsx setlocal sw=2 ts=2 foldmethod=syntax
