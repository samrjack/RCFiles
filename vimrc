set nocompatible

" Add paths
" set runtimepath^=~/.vim/bundle/*
set runtimepath^=~/.vim/colors/*
set dictionary+=/usr/share/dict/words

" Colors
colorscheme desert
syntax enable         " enable syntax processing
filetype indent plugin on

" Tabs
" set noexpandtab     " Make sure that every file uses real tabs
set shiftround        " Round indent to multiples of 'shiftwidth'
set smartindent       " Do smart indenting
set autoindent        " Copy indent from current line over to new line
set tabstop=4         " Number of visual spaces per TAB
set softtabstop=4     " Number of spaces in a tab when editing
set shiftwidth=4      " Number of spaces the >> indents lines
set softtabstop=4 
set expandtab 

" UI
set number            " Shows line numbers
set relativenumber    " Shows the line number relative to the cursor
set showcmd           " Shows the most recent command
set cursorline        " Highlight current line
filetype indent on    " Load filetype-specific indent files
set wildmenu          " Visual autocomplete for command menue
set lazyredraw        " Redraw only when needed
set showmatch         " Highlight matching [{()}]

" Searching
set incsearch         " Search as characters are entered
set hlsearch          " Highlight matches
set path+=**          " Commands like find search in all sub directories
command! MakeTags !ctags -R

" Folding
set foldenable        " enable folding
set foldnestmax=10    " prevents too many folds
" Space opens and closes folds
nnoremap <space> za   
set foldmethod=syntax
set foldlevelstart=10

" Remaps
" Highlight last inserted text
nnoremap gV `[v`]
" jk is escape
inoremap jk <esc>
" Toggle gundo
nnoremap <leader>u :GundoToggle<CR>
" Save session (open again with vim -S)
nnoremap <leader>s :mksession<CR>

autocmd Filetype verilog setlocal softtabstop=2 tabstop=2 shiftwidth=2 expandtab foldmethod=indent
autocmd Filetype systemverilog setlocal softtabstop=2 tabstop=2 shiftwidth=2 expandtab foldmethod=indent
autocmd Filetype haskell setlocal softtabstop=4 tabstop=4 shiftwidth=4 expandtab foldmethod=indent
