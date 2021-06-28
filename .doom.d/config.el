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
(setq doom-theme 'doom-one)


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
(setq save-interprogram-paste-before-kill t)
(setq select-enable-clipboard nil)
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))


;; (setq-default left-margin-width 1)
;; (set-window-buffer nil (current-buffer))

(define-key evil-normal-state-map (kbd "C-n") 'dired-sidebar-toggle-sidebar)

;; Checkout out https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/ for configuration example
;; (map! :leader
;;       (:prefix-map ("a" . "personal")
;;        (:prefix-map ("a" . "test2")
;;         (:prefix ("a" . "test")
;;          :desc "a test function to see if this works" "j" #'org-journal-new-entry
;;          :desc "Search journal entry" "s" #'org-journal-search))))

;; Disable flycheck mode on load. Can be re-enabled in a buffer with SPC t f
(after! flycheck
  (global-flycheck-mode -1))

(after! projectile
  (setq projectile-track-known-projects-automatically nil))

(load! "settings/org-settings.el")
(load! "settings/evil-snipe-settings.el")

