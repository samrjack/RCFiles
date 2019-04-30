set nocompatible      " VI improve allowed
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'VundleVim/Vundle.vim'

""""""""""""""""""" All plugins here """""""""""""""""""
""""" Syntax """""
    " Syntax highlighting for perl-mason
    Plugin 'aming/vim-mason'

    " Haskell folding capability
    Plugin 'vim-scripts/haskellFold'

    " JS React highlighting
    " Plugin 'pangloss/vim-javascript'
    " Plugin 'maxmellon/vim-jsx-pretty'
    Plugin 'pangloss/vim-javascript'
    Plugin 'mxw/vim-jsx'

""""" Navigation """""
    " Nerd tree directory navigator
    Plugin 'scrooloose/nerdtree'

    " Fuzzy File Finder
    Plugin 'kien/ctrlp.vim'

""""" Viewing """""
    " Differenc color parentheses
    Plugin 'kien/rainbow_parentheses.vim'
    Plugin 'luochen1990/rainbow'

    " Indentation guide
    Plugin 'nathanaelkane/vim-indent-guides'

    " Dracula theme
    Plugin 'dracula/vim'
    
    " Badwolf theme
    Plugin 'sjl/badwolf'

    " Gruvbox theme
    Plugin 'morhetz/gruvbox'
    
    " Colorful status bar
    Plugin 'itchyny/lightline.vim'
    
    " Hex Color viewing
    Plugin 'gu-fan/colorv.vim'

""""" Editing """""
    " Undo tree plugin
    Plugin 'mbbill/undotree'

    " Allow for surrounding notation
    Plugin 'tpope/vim-surround'
    Plugin 'tpope/vim-repeat'
    
    " Allow for easy commenting
    Plugin 'tomtom/tcomment_vim'

""""" GIT """""
    " NERDTree git plugin
    Plugin 'Xuyuanp/nerdtree-git-plugin'

    " Git
    Plugin 'airblade/vim-gitgutter'
    Plugin 'tpope/vim-fugitive'
    Plugin 'tpope/vim-rhubarb'

""""" Other """""
    " Calandar
    Plugin 'mattn/calendar-vim'

""""""""""""""""""" Plugins end """""""""""""""""""

call vundle#end()            " All plugins before here
filetype plugin indent on    " turn filetypes back on
filetype on

""""""""""""""""""" Configure Plugin values and mappings """""""""""""""""""
""""" Navigation """""
    " Nerd tree options
    " Use CTRL-n to open NERDtree
    map <C-n> :NERDTreeToggle<CR>
    " Undo tree options
    " Use leader z to open undo
    map <leader>z :UndotreeToggle<CR>

    " Resize NERDTree window to be bigger than standard
    :let g:NERDTreeWinSize=80

    " Don't let NERDTree quit after selecting a file
    let NERDTreeQuitOnOpen=0

    " Open NERDTree automatically when vim starts up on opening a directory
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
    
    " Fuzzy finding files
    cnoreabbrev ep CtrlP

""""" Viewing """""
    " Rainbow parentheses
    nnoremap <leader>r :RainbowParenthesesToggleAll<CR>

    " Indentation guide
    nnoremap <leader>i :IndentGuidesToggle<CR>
    
    " Colorschemes
    cnoreabbrev badwolf colorscheme badwolf
    cnoreabbrev dracula colorscheme dracula
    cnoreabbrev gruvbox colorscheme gruvbox

    " To see lightline
    set laststatus=2
    set noshowmode

""""" Editing """""
    " Undo Tree
    nnoremap <Leader>u :UndotreeToggle<CR>

""""" GIT """""
    " Git plugin settings
    set updatetime=2000
    let g:gitgutter_map_keys = 0
    nnoremap <Leader>gb :Gblame<CR>
    nnoremap <Leader>gx :GitGutterSignsToggle<CR>
    nnoremap <Leader>gh :GitGutterLineHighlightsToggle<CR>
    nnoremap <Leader>gn :GitGutterNextHunk<CR>
    nnoremap <Leader>gp :GitGutterPrevHunk<CR>
    nnoremap <Leader>gf :GitGutterFold<CR>
    nnoremap <Leader>ga :GitGutterStageHunk<CR>
    nnoremap <Leader>gu :GitGutterUndoHunk<CR>
    nnoremap <Leader>gv :GitGutterPreviewHunk<CR>

""""" Other """""
    " Calandar
    nnoremap <Leader>c :Calendar<CR>

""""""""""""""""""" Environment Values """""""""""""""""""
""""" Colors """""
    " colorscheme dracula
    colorscheme desert
    syntax enable          " Enable syntax processing
    set colorcolumn=100    " Hilights the 100th column
    " Sets column color to grey
    highlight ColorColumn ctermbg=8

""""" Tabs """""
    set smarttab           " Indents to the correct spot first time
    set shiftwidth=4       " Code defaults to 4 space indents
    set tabstop=4          " Number of visual spaces per TAB
    set softtabstop=4      " Number of spaces in a tab when editing
    set autoindent         " Automatically indents when enter is pressed
    set expandtab          " Makes all tabs into spaces

""""" UI """""
    " set relativenumber     " Show line numbers relative to cursor
    set number             " Shows the line number of the current line.
    set showcmd            " Shows the most recent command
    set cursorline         " Highlight current line
    filetype indent on     " Load filetype-specific indent files
    set wildmenu           " Visual autocomplete for command menue
    set lazyredraw         " Redraw only when needed
    set showmatch          " Highlight matching [{()}]
    set ruler

""""" Searching """""
    set incsearch          " Search as characters are entered
    set hlsearch           " Highlight search matches
    set dictionary=/usr/share/dict/words "dictionary for searching

""""" Folding """""
    set foldenable         " Enables code folding
    set foldnestmax=100    " Prevents too many folds
    nnoremap <space> za
                           " Set space to close folds
    set foldmethod=indent  " Default to using indentation for folds
    set foldlevelstart=20  " Start with all folds open

""""" Remaps """""
    nnoremap gV `[v`]
                           " Highlight last inserted text
    noremap <C-i> ^
                           " Go to beginning of line
    noremap <C-a> $
                           " Go to end of line
    noremap <C-j> kddpkJ
                           " Joins the previous line to the end of the current line (J in the oppisite direction)
    noremap <C-l> :redraw<CR>:syntax sync fromstart<CR>
                           " Changes the ctrl-l redraw to also redraw syntax
                           " highlighting

""""" Insert mode moveing """""
    inoremap <C-h> <Right>
    inoremap <C-j> <Down>
    inoremap <C-k> <Up>
    inoremap <C-l> <Right>
    inoremap jk <esc>
                           " jk kes act as esc together

    set backspace=indent,eol,start
    set redrawtime=10000

""""" Meta changes """""
    silent !mkdir ~/.swap > /dev/null 2>&1
    set backupdir=~/.swap//,.,/tmp//
    set directory=~/.swap//,.,/tmp//

""""" File Specific changes """""
    "autocmd Filetype javascript.jsx setlocal sw=2 ts=2 foldmethod=syntax
    autocmd Filetype vim setlocal sw=4 ts=4 foldmethod=indent

    "Note, perl automatically sets foldmethod in the syntax file
    autocmd Syntax c,cpp,vim,xml,html,xhtml setlocal foldmethod=syntax
    autocmd Syntax c,cpp,vim,xml,html,xhtml,perl normal zR
