set nocompatible      " VI improve allowed
filetype off

function! IsVundleInstalled()
    let vundle_readme=expand('~/.vim/bundle/Vundle.vim/README.md')
    return filereadable(vundle_readme)
endfunction
" Install vundle if needed
	let vundlePreviouslyInstalled=1
	if !IsVundleInstalled() && executable('git')
	    echo "Installing Vundle.."
	    echo ""
	    silent !mkdir -p ~/.vim/bundle
	    silent !git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	    let vundlePreviouslyInstalled=0
	endif

    if IsVundleInstalled()
        set rtp+=~/.vim/bundle/Vundle.vim/
        call vundle#rc()

        " let Vundle manage Vundle, required
        Plugin 'VundleVim/Vundle.vim'
        if vundlePreviouslyInstalled == 0
            echo "Installing Bundles, please ignore key map error messages"
            echo ""
            :PluginInstall
        endif
    endif
" END - Setting up Vundle

" set the runtime path to include Vundle and initialize
if IsVundleInstalled()
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

    " let Vundle manage Vundle
    Plugin 'VundleVim/Vundle.vim'

    """"""""""""""""""" All plugins here """""""""""""""""""
    """"" Syntax """""
        " Org mode
        Plugin 'jceb/vim-orgmode'

        " Syntax highlighting for perl-mason
        Plugin 'aming/vim-mason'

        " Haskell folding capability
        Plugin 'vim-scripts/haskellFold'

        " JS React highlighting
        " Plugin 'pangloss/vim-javascript'
        " Plugin 'maxmellon/vim-jsx-pretty'
        Plugin 'pangloss/vim-javascript'
        Plugin 'mxw/vim-jsx'

        " Larg syntax highlighting package
        Plugin 'sheerun/vim-polyglot'

        " Tmux environment
        Plugin 'tmux-plugins/vim-tmux'

        " Allow Ansi color escape characters to be shown
        Plugin 'powerman/vim-plugin-AnsiEsc'
     
        " Python IDE style completion
        " Plugin 'python-mode/python-mode'

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
        
        " Colorful status bar
        Plugin 'itchyny/lightline.vim'
        
        " Hex Color viewing
        Plugin 'gu-fan/colorv.vim'

        " Terminal escape color viewing
        Plugin 'chrisbra/Colorizer'
        
        """ Themes
            " Dracula theme
            Plugin 'dracula/vim'
            
            " Badwolf theme
            Plugin 'sjl/badwolf'

            " Gruvbox theme
            Plugin 'morhetz/gruvbox'

            " Green forest dark theme
            Plugin 'sainnhe/vim-color-forest-night'

            " Sacred forest
            Plugin 'KKPMW/sacredforest-vim'

            " Nord colorscheme
            Plugin 'arcticicestudio/nord-vim'

            " Lightline themes
            Plugin 'sainnhe/lightline_foobar.vim'

    """"" Editing """""
        " Undo tree plugin
        Plugin 'mbbill/undotree'

        " Allow for surrounding notation
        Plugin 'tpope/vim-surround'
        Plugin 'tpope/vim-repeat'
        
        " Allow for easy commenting
        Plugin 'tomtom/tcomment_vim'

        " Commenting plugin
        Plugin 'scrooloose/nerdcommenter'

        " Incrementer works with dates too
        Plugin 'tpope/vim-speeddating'

        " Working with CSVs
        Plugin 'chrisbra/csv.vim'

        " Multiple cursors
        " Plugin 'terryma/vim-multiple-cursors'

    """"" GIT """""
        " NERDTree git plugin
        Plugin 'Xuyuanp/nerdtree-git-plugin'

        " Git
        Plugin 'airblade/vim-gitgutter'
        Plugin 'tpope/vim-fugitive'
        Plugin 'tpope/vim-rhubarb'
        
        " Git Messenge
        Plugin 'rhysd/git-messenger.vim'

    """"" Other """""
        " Calandar
        Plugin 'mattn/calendar-vim'

        " Save file view after exiting file
        Plugin 'vim-scripts/restore_view.vim'

        " Access Databases
        Plugin 'tpope/vim-dadbod'

    """"""""""""""""""" Plugins end """""""""""""""""""

    call vundle#end()            " All plugins before here
endif

filetype plugin indent on    " turn filetypes back on
filetype on

""""""""""""""""""" Configure Plugin values and mappings """""""""""""""""""
if IsVundleInstalled()
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
        let g:rainbow_active = 0
        " Rainbow parentheses
        " nnoremap <leader>r :RainbowParenthesesToggleAll<CR>
        nnoremap <leader>r :RainbowToggle<CR>

        " Indentation guide
        nnoremap <leader>i :IndentGuidesToggle<CR>
        
        " To see lightline
        set laststatus=2
        set noshowmode
        let g:lightline =  {
            \     'colorscheme': 'sacredforest_alter'
            \  }

    """"" Editing """""
        " Undo Tree
        nnoremap <Leader>u :UndotreeToggle<CR>

        " Commenter settings
            " Add spaces after comment delimiters by default
            let g:NERDSpaceDelims = 1

            " Use compact syntax for prettified multi-line comments
            let g:NERDCompactSexyComs = 1

            " Align line-wise comment delimiters flush left instead of following code indentation
            let g:NERDDefaultAlign = 'left'

            " Set a language to use its alternate delimiters by default
            let g:NERDAltDelims_java = 1

            " Add your own custom formats or override the defaults
            " let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

            " Allow commenting and inverting empty lines (useful when commenting a region)
            let g:NERDCommentEmptyLines = 1

            " Enable trimming of trailing whitespace when uncommenting
            let g:NERDTrimTrailingWhitespace = 1

            " Enable NERDCommenterToggle to check all selected lines is commented or not
            let g:NERDToggleCheckAllLines = 1

        " CSV table manipulation
            " Arrange the table so it's columns align
            nnoremap <Leader>ta :%ArrangeCol<CR>

    """"" GIT """""
        " Git plugin settings
        set updatetime=500
        let g:gitgutter_map_keys = 0
        let g:git_messenger_always_into_popup = 1

        " Function for toggling on and off git blame so I don't need to
        " directly close it.
        function! s:ToggleBlame()
            if &l:filetype ==# 'fugitiveblame'
                close
            else
                Gblame
            endif
        endfunction
            
        nnoremap <Leader>gb :call <SID>ToggleBlame()<CR>
        nnoremap <Leader>gd :Gdiff<CR>
        nnoremap <Leader>gs :Gstatus<CR>
        nnoremap <Leader>ge :Gedit<CR>
        nnoremap <Leader>gx :GitGutterSignsToggle<CR>
        nnoremap <Leader>gh :GitGutterLineHighlightsToggle<CR>
        nnoremap <Leader>gn :GitGutterNextHunk<CR>
        nnoremap <Leader>gp :GitGutterPrevHunk<CR>
        nnoremap <Leader>gf :GitGutterFold<CR>
        nnoremap <Leader>ga :GitGutterStageHunk<CR>
        nnoremap <Leader>gu :GitGutterUndoHunk<CR>
        nnoremap <Leader>gv :GitGutterPreviewHunk<CR>
        nnoremap <Leader>gm :GitMessenger<CR>

    """"" Other """""
        " Calandar
        nnoremap <Leader>c :Calendar<CR>

        " Saved view parameters
        set viewoptions=cursor,folds,slash,unix
        " let g:skipview_files = ['*\.vim']
endif

""""""""""""""""""" Environment Values """""""""""""""""""
""""" Colors """""
    " enables true color for themes
        if exists('+termguicolors')
            let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
            let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
            set termguicolors
        endif

    " Set colorscheme
        try
            colorscheme forest-night
        catch
            try
                colorscheme gruvbox
            catch
                try
                    colorscheme dracula
                catch
                    try
                        colorscheme desert
                    catch
                        echo "no colorschemes avaliable"
                    endtry
                endtry
            endtry
        endtry

    syntax enable          " Enable syntax processing
    
    if exists('colorcolumn')
        set colorcolumn=120    " Highlights the nth column
        " Sets column color to grey
        highlight ColorColumn ctermbg=8
    endif

""""" Indentation """""
    set smarttab           " Indents to the correct spot first time
    set shiftwidth=4       " Code defaults to 4 space indents
    set tabstop=4          " Number of visual spaces per TAB
    set softtabstop=4      " Number of spaces in a tab when editing
    set autoindent         " Automatically indents when enter is pressed
    set expandtab          " Makes all tabs into spaces

""""" UI """""
    " set relativenumber     " Show line numbers relative to cursor
    set confirm            " Asks to save before quiting a file instead of preventing quit
    set number             " Shows the line number of the current line
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
    set foldignore=""      " Doesn't ignore any characters when deciding fold level

""""" Remaps """""
    nnoremap gV `[v`]
                           " Highlight last inserted text
    noremap <C-j> kddpkJ
                           " Joins the previous line to the end of the current line (J in the oppisite direction)
    noremap <C-l> :redraw<CR>:syntax sync fromstart<CR>
                           " Changes the ctrl-l redraw to also redraw syntax
                           " highlighting
    nnoremap <Leader>s :source $MYVIMRC<CR>
                           " Re-sources the vimrc file.
    nnoremap <Leader>S :source %<CR>
                           " Sources the current file. Used when testing new
                           " features.

""""" Insert mode moveing """""
    inoremap <C-h> <Right>
    inoremap <C-j> <Down>
    inoremap <C-k> <Up>
    inoremap <C-l> <Right>
    inoremap jk <esc>
                           " jk kes act as esc together

    set backspace=indent,eol,start
    set redrawtime=10000
    set lazyredraw

""""" Meta changes """""
    silent !mkdir ~/.swap > /dev/null 2>&1
    set backupdir=~/.swap//,.,/tmp//
    set directory=~/.swap//,.,/tmp//

""""" File Specific changes """""
    " Makes command 'TurnOnScratchBuffer' force the current buffer to become a
    " scratch buffer.
    command! -bar TurnOnScratchBuffer setlocal buftype=nofile bufhidden=hide noswapfile
    command! -bar TurnOffScratchBuffer setlocal buftype= bufhidden= swapfile
    command! -bar NewScratch new | TurnOnScratchBuffer

    augroup remove_quite_prompt 
        autocmd!
        autocmd StdinReadPre * TurnOnScratchBuffer
        autocmd VimEnter * 
            \   if @% == '' && &buftype == ''
            \ |     TurnOnScratchBuffer
            \ | endif
        autocmd BufWritePost * ++nested
            \   if (empty(bufname()) || bufname() == '-stdin-') && &buftype == 'nofile'
            \ |     TurnOffScratchBuffer
            \ |     setlocal nomodified
            \ |     edit <afile>
            \ | endif
    augroup END

    " augroup AutoSaveFolds
    "     autocmd!
    "     autocmd BufWinLeave * mkview
    "     autocmd BufWinEnter * silent loadview
    " augroup END

    augroup filetype_syntax_changes
        autocmd!
        " Set default file type for files without so that they can have basic
        " hilighting functionality.
        autocmd BufNewFile,BufRead * if &ft == '' | setlocal filetype=c | endif

        "autocmd Filetype javascript.jsx setlocal sw=2 ts=2 foldmethod=syntax
        autocmd Filetype vim setlocal sw=4 ts=4 foldmethod=indent

        " Note, perl automatically sets foldmethod in the syntax file
        autocmd Syntax c,cpp,vim,xml,html,xhtml setlocal foldmethod=syntax
        autocmd Syntax c,cpp,vim,xml,html,xhtml,perl normal zR
    augroup END

