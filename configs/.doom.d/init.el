;;; init.el -*- lexical-binding: t; -*-

(doom!
;;; Modules to load

       :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       chinese           ; spend your 3 hours a week in Emacs
       ;;japanese          ; ah, a man of culture
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company +childframe); the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy               ; a search engine for love and life
       (vertico +icons   ; tomorrow's search engine
                +childframe)

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +ascii     ;💩 in the text
              +github
              +unicode)
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra             ; Discount modality for mythological beast hunters
       indent-guides     ; highlighted indent columns
       (ligatures        ; ligatures and symbols to make your code pretty again
        +extra
        +hasklig)
       minimap           ; a map for lost programmers
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +defaults)
       ;;tabs              ; a tab bar for Emacs
       (treemacs +lsp)   ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       (vc-gutter +pretty); vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       ;;fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icon)     ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       shell             ; simple shell REPL for Emacs
       term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       grammar           ; tasing grammar mistake every you make
       (spell +aspell    ; tasing you for misspelling mispelling
              +everywhere)
       syntax            ; tasing you for every semicolon you forget

       :tools
       ;;ansible           ; Allow silly people to focus on silly things
       biblio            ; writes a PhD for you (citation needed)
       (debugger +lsp)   ; stepping through code, to help you add bugs
       direnv            ; integrates direnv into Emacs
       (docker +lsp)     ; yo dawg, I heard you like OSes, so I...
       editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)   ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup +offline  ; navigate your code and its documentation
               +dictionary)
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; No sweatshop is complete without child processes
       rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       tree-sitter       ; syntax and parsing, sitting in a tree
       upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       (cc +lsp          ; C/C++/Obj-C madness
           +tree-sitter)
       (clojure +lsp)    ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;(dart +flutter)   ; paint ui and not much else
       data              ; config/data formats
       ;;dhall             ; config as code
       (elixir +lsp      ; erlang done right
               +tree-sitter)
       (elm +lsp         ; care for a cup of TEA?
            +tree-sitter)
       emacs-lisp        ; a parsel-tongue for the oldest serpent
       (erlang +lsp)     ; an elegant language for a more civilized age
       (ess +stan)       ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       (go +lsp          ; the hipster dialect
           +tree-sitter)
       ;;graphql           ; give queries a REAT
       (haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       (java +lsp        ; the poster child for carpal tunnel syndrome
             +tree-sitter)
       (javascript +lsp  ; all(hope(abandon(ye(who(enter(here))))))
                   +tree-sitter)
       (json +lsp        ; At least it ain't XML
             +tree-sitter)
       ;;(julia +lsp)      ; a better, faster MATLAB
       (kotlin +lsp)     ; a better, slicker Java(Script)
       (latex +lsp       ; writing papers in Emacs has never been so fun
              +latexmk
              +cdlatex
              +fold)
       ;;lean              ; for folks with too much to prove
       ledger            ; an accounting system in Emacs
       (lua +lsp         ; one-based indices? one-based indices
            +fennel
            +moonscript)
       (markdown +grip)  ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       (ocaml +lsp       ; an objective camel
              +tree-sitter)
       (org +brain       ; organize your plain life in plain text
            +dragndrop
            +gnuplot
            +ipython
            +journal
            +jupyter
            +noter
            +pandoc
            +pomodoro
            +present
            +pretty
            +roam2)
       ;;php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       (purescript +lsp) ; javascript, but functional
       (python +lsp      ; beautiful is better than ugly
               +poetry
               +pyenv
               +pyright
               +tree-sitter)
       ;;qt                ; the 'cutest' gui framework ever
       (racket +lsp      ; a DSL for DSLs
               +xp)
       ;;raku              ; the artist formerly known as perl6
       (rest +jq)        ; Emacs as a REST client
       rst               ; ReST in peace
       (ruby +rails      ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
             +rvm
             +rbenv
             +lsp
             +tree-sitter)
       (rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scala +lsp)      ; java, but good
       scheme            ; a fully conniving family of lisps
       (sh +lsp          ; she sells {ba,z,fi}sh shells on the C xor
           +tree-sitter)
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp         ; the tubes
            +tree-sitter)
       (yaml +lsp)       ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       (mu4e +gmail)     ; the great filter Hanson hadn't anticipated
       ;;notmuch           ; closest Emacs will ever be to multi-threaded
       (wanderlust +gmail); to boldly go where no mail has gone before

       :app
       calendar          ; Watch your missed deadlines in real time
       ;;emms              ; a media player for music no one's heard of
       ;;everywhere        ; leave Emacs!? You must be joking
       irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       twitter           ; twitter client https://twitter.com/vnought

       :config
       (default +bindings; reasonable defaults for reasonable people
                +smartparens)
       ;;literate          ; Disguise your config as poor documentation

)
