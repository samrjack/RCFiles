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

;; Used to view VERY large files faster
(package! vlf :disable t) ; disabled on startup to not impact load time.
