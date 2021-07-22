;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Samuel Jackson"
      user-mail-address "dsiq3g@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme (if (display-graphic-p)'doom-one 'doom-dracula))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Prevents system clipboard from being accidentially overwritten.
;; Must now write to register "+ to write to system clipboard.

;; Some paste related settings.
;; TODO I want to find a way to copy and paste to clipboard without
;; having to go through "+ directly.
(setq save-interprogram-paste-before-kill t
      select-enable-clipboard nil)
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Resize all windows when a new one comes in so they have
;; equal space.
(setq-default window-combination-resize t
;; changes the cursor to be the size of a gliph in the buffer.
              x-stretch-cursor t)

;; Let the undo buffer use up to 100Mb
(setq undo-limit 100000000
;; Make undo revert smaller sections of text instead of all text
;; added while in insert mode.
      evil-want-fine-undo t
;; leave some space at the bottom while scrolling down so the
;; cursor isn't hugging the bottom edge.
      scroll-margin 2)

;; Makes it so movement keys stop at camlecase sub words.
(global-subword-mode 1)

;; (setq-default left-margin-width 1)
;; (set-window-buffer nil (current-buffer))

(define-key evil-normal-state-map (kbd "C-n") 'dired-sidebar-toggle-sidebar)

(defun print-point-position ()
  "Print the position of point to the message console."
  (interactive)
  (message (number-to-string (point))))

(map! :leader
      (:prefix-map ("a" . "Additional")
        (:desc "Point's position" "p" #'print-point-position)))
;;        (:prefix-map ("a" . "test2")
;;         (:prefix ("a" . "test")
;;          :desc "a test function to see if this works" "j" #'org-journal-new-entry
         ;; :desc "Search journal entry" "s" #'org-journal-search))))
;;
;; Set the correct dictionary for spell check.
(setq ispell-dictionary "en")
(remove-hook! (org-mode markdown-mode rst-mode asciidoc-mode latex-mode) #'writegood-mode)
(add-hook 'writegood-mode-hook 'writegood-passive-voice-turn-off)
(map! :leader
      :desc "Write good mode"
      "t W" #'writegood-mode)

;; Disable flycheck mode on load. Can be re-enabled in a buffer with SPC t f
(remove-hook! (doom-first-buffer) #'global-flycheck-mode)

(after! projectile
  (setq projectile-track-known-projects-automatically nil))

(setq custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file) (load custom-file))

(setq which-key-idle-delay 0.5)
;; enables nested snippets
(setq yas-triggers-in-field t)

(load! "settings/package-settings.el")
(load! "settings/mode-line-settings.el")
(load! "settings/org-settings.el")
(load! "settings/org-template-settings.el")
(load! "settings/evil-settings.el")
(load! "settings/evil-snipe-settings.el")
