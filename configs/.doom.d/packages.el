;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; directory management
(package! dired-sidebar)
(package! dired+)
(package! dired-subtree)

;; Themes
(package! dracula-theme)
(package! theme-magic)

;; Org mode related stuff
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

(package! systemd)

(package! vimrc-mode)

(package! scad-mode)

(package! ess-view)

(package! graphviz-dot-mode)

;; e-reader
(package! calibredb)
(package! nov)

;; Fun stuff
(package! xkcd)

(package! selectric-mode)

;; Presentation/showing emacs
(package! command-log-mode)
(package! gif-screencast)
(package! keycast)

(package! info-colors)

(package! discover-my-major)

(package! spray)

(package! elcord)

(package! guix)

(package! steam)

(package! 2048-game)

(package! mines)

(package! minesweeper)

(package! sudoku)

(package! threes)

;; Used to view VERY large files faster
(package! vlf :disable t) ; disabled on startup to not impact load time.

;; [[file:~/.config/doom/config.org::*Very large files][Very large files:1]]
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)
