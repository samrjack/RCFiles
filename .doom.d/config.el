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

(define-key evil-normal-state-map (kbd "C-n") '+treemacs/toggle)
;; Checkout out https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/ for configuration example

;; (map! :leader
;;       (:prefix-map ("a" . "personal")
;;        (:prefix-map ("a" . "test2")
;;         (:prefix ("a" . "test")
;;          :desc "a test function to see if this works" "j" #'org-journal-new-entry
;;          :desc "Search journal entry" "s" #'org-journal-search))))

;;; Org mode bindings
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; Agenda
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/todo.org"))

;; Example of org capture templates
;; for example text, see https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el
;; (after! org
;;   (setq org-capture-templates '(
;;     ("t" "Todo" entry () "" :prepend t)
;;     ("k" "Kudos" entry () "" :prepend t)
;;     ("f" "Followup" entry () "" :prepend t)
;;     ("p" "Personal" entry () "" :prepend t)
;;     ("P" "Project" entry () "" :prepend t)
;;     ("j" "Journal" entry () "" :prepend t)
;;   )))



;; '(
;;   ("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* [ ] %?\n%i\n%a" :prepend t)
;;           ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?\n%i\n%a" :prepend t)
;;           ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
;;            "* %U %?\n%i\n%a" :prepend t)

;;           ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
;;           ;; {todo,notes,changelog}.org file is found in a parent directory.
;;           ;; Uses the basename from `+org-capture-todo-file',
;;           ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
;;           ("p" "Templates for projects")
;;           ("pt" "Project-local todo" entry  ; {project-root}/todo.org
;;            (file+headline +org-capture-project-todo-file "Inbox")
;;            "* TODO %?\n%i\n%a" :prepend t)
;;           ("pn" "Project-local notes" entry  ; {project-root}/notes.org
;;            (file+headline +org-capture-project-notes-file "Inbox")
;;            "* %U %?\n%i\n%a" :prepend t)
;;           ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
;;            (file+headline +org-capture-project-changelog-file "Unreleased")
;;            "* %U %?\n%i\n%a" :prepend t)

;;           ;; Will use {org-directory}/{+org-capture-projects-file} and store
;;           ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
;;           ;; support `:parents' to specify what headings to put them under, e.g.
;;           ;; :parents ("Projects")
;;           ("o" "Centralized templates for projects")
;;           ("ot" "Project todo" entry
;;            (function +org-capture-central-project-todo-file)
;;            "* TODO %?\n %i\n %a"
;;            :heading "Tasks"
;;            :prepend nil)
;;           ("on" "Project notes" entry
;;            (function +org-capture-central-project-notes-file)
;;            "* %U %?\n %i\n %a"
;;            :heading "Notes"
;;            :prepend t)
;;           ("oc" "Project changelog" entry
;;            (function +org-capture-central-project-changelog-file)
;;            "* %U %?\n %i\n %a"
;;            :heading "Changelog"
;;            :prepend t)))

;; Pomodoro
(after! org
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15))

(after! org
  (setq org-pomodoro-play-sounds t
        ;; org-pomodoro-start-sound-p f
        ;; org-pomodoro-ticking-sound-p f
        org-pomodoro-killed-sound-p t
        org-pomodoro-finished-sound-p t
        org-pomodoro-short-break-sound-p t
        org-pomodoro-long-break-sound-p t))

;; need to find (or make) some better alert audio files.
;; (setq ;org-pomodoro-start-sound ()
;;       ;org-pomodoro-ticking-sound ()
;;       org-pomodoro-killed-sound ()
;;       org-pomodoro-finished-sound ()
;;       org-pomodoro-short-break-sound ()
;;       org-pomodoro-long-break-sound ())

;;; config.el ends here
