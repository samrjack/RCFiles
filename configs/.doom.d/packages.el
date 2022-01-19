;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Themes
(package! dracula-theme)
(package! theme-magic)

;; directory management
(package! dired-sidebar)
(package! dired+)
(package! dired-subtree)

(package! dired-sidebar)

(package! discover-my-major)

;; [[file:~/.config/doom/config.org::*Very large files][Very large files:1]]
(package! vlf :recipe (:host github :repo "m00natic/vlfi"))

(package! info-colors)

(package! ess-view)

(package! origami)

(package! blamer)

(package! diff-ansi :recipe (:host gitlab :repo "ideasman42/emacs-diff-ansi"))

(package! elcord)

(package! spray)

(package! keycast)

(package! gif-screencast)

(package! command-log-mode)

(package! kubernetes)
(package! kubernetes-evil)

(package! kubernetes-helm)

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

;; e-reader
(package! calibredb)
(package! nov)

(package! scad-mode)

(package! prettier-js)

(package! vimrc-mode)

(package! graphviz-dot-mode)

(package! systemd)

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

(package! fireplace)

(package! power-mode :recipe (:host github :repo "elizagamedev/power-mode.el"))

(unpin! dired-git-info)
