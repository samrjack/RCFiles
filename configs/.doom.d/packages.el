;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Themes
(package! dracula-theme)
(package! theme-magic)

(package! dired-sidebar)
(package! dired+)
(package! dired-subtree)

(package! dired-sidebar)

(package! dirtree)
(package! ztree)
(package! dir-treeview)

(package! zoom-window)

(package! centered-cursor-mode)

(package! discover-my-major)

(package! marginalia)

(package! embark)

;; [[file:~/.config/doom/config.org::*Very large files][Very large files:1]]
(package! vlf :recipe (:host github :repo "m00natic/vlfi"))

(package! info-colors)

(package! ess-view)

(package! origami)

(package! blamer)

(package! diff-ansi)

(package! encrypt-region)

(package! bang :recipe (:host github :repo "Arian-D/bang.el"))

(package! logos)

(package! lorem-ipsum)

(package! rg)

(package! elcord)

(package! spray :recipe (:host nil :repo "https://git.sr.ht/~iank/spray"))

(package! keycast)

(package! gif-screencast)

(package! command-log-mode)

(package! kubernetes)
(package! kubernetes-evil)

(package! k8s-mode)

(package! kubernetes-helm)

(package! kubel)

(package! bluetooth)

(package! mode-minder :recipe (:host github :repo "jdtsmith/mode-minder"))

(package! guix)

(package! org-reverse-datetree)
;(package! ox-gfm) ; Causing problems with pandoc at the moment.
(package! org-ref)
(package! org-chef)
(package! org-super-agenda)
(package! org-fragtog)
(package! org-pretty-tags)

;; Clojure related packages
(package! clojure-mode)
(package! clojure-mode-extra-font-locking)
(package! clj-refactor)
(package! cider)

(unpin! org-roam)

(package! org-roam-ui)

(package! org-present)

(package! visual-fill-column :recipe (:host nil :repo "https://codeberg.org/joostkremers/visual-fill-column"))

(package! org-chef)

(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))

(package! org-gantt :recipe (:host github :repo "swillner/org-gantt"))

(unpin! tree-sitter-langs)

(package! ts-fold :recipe (:host github :repo "emacs-tree-sitter/ts-fold"))

;; e-reader
(package! calibredb)
(package! nov)

(package! project-rootfile)

(package! scad-mode)

(package! geiser)
(package! geiser-guile)
(package! geiser-mit)
(package! geiser-racket)

(package! prettier-js)

(package! gitlab-ci-mode)

(package! vimrc-mode)

(package! graphviz-dot-mode)

(package! systemd)

(package! company-tabnine)

;; Fun stuff
(package! xkcd)

(package! selectric-mode)

(package! steam)

(package! 2048-game)

(package! mines)

(package! minesweeper)

(package! sudoku)

(package! threes)

(package! pacmacs)

(package! wordel :recipe (:host github :repo "progfolio/wordel"))

(package! fireplace)

(package! power-mode :recipe (:host github :repo "elizagamedev/power-mode.el"))

;; (unpin! dired-git-info)

;; This is supposed to fix the issue but doesn't seem to work on macOS. Not sure why.
;; (package! org-mode :pin "971eb6885ec996c923e955730df3bafbdc244e54")

(unpin! straight)

(package! ts-fold :recipe
  (:host github
   :repo "samrjack/ts-fold"
   :branch "golang"))

(package! zoom-window :recipe
  (:host github
   :repo "samrjack/zoom-window"
   :branch "handlePerspRename"))
