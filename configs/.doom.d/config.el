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

;; (setq-default left-margin-width 1)
;; (set-window-buffer nil (current-buffer))

;; Let the undo buffer use up to 100Mb
(setq undo-limit 100000000)

;; Make undo revert smaller sections of text instead of all text
;; added while in insert mode.
(setq evil-want-fine-undo t)

;; leave some space at the bottom while scrolling down so the
;; cursor isn't hugging the bottom edge.
(setq scroll-margin 2)

;; Makes it so movement keys stop at camlecase sub words.
(global-subword-mode 1)

;; Make searches case sensitive
(setq-default case-fold-search nil)

(map! :n "g /" #'which-key-show-top-level
      :n "g C-/" #'which-key-show-full-major-mode
      :n "g M-/" #'which-key-show-minor-mode-keymap)
(setq which-key-idle-delay 0.5)

(map! :n "C-n" #'dired-sidebar-toggle-sidebar)

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

;; enables nested snippets
(setq yas-triggers-in-field t)

(setq web-mode-script-padding standard-indent)
(setq web-mode-style-padding standard-indent)
(setq web-mode-block-padding standard-indent)
(setq web-mode-part-padding standard-indent)

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key bining in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    ('keycast-command :inherit doom-modeline-debug :height 0.9)
    ('keycast-key :inherit custom-modified :height 1.1 :weight bold)))

(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args '("--quality" "3" "-1" ,(string-trim-right (shell-command-to-string "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string "\n+" "\n"
                               (mapconcat (lambda (c) (if (listp (cdr c))) (cadr c)))
                               'utf-8
                               "/tmp/doom-color-theme")))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))

;; (use-package! vlf-setup
;;   :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; (use-package! ox-gfm
;;   :after org)

(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'info-selection-hook 'info-colors-fontify-node)

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(use-package! nov ; Novel reading
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)
  (defun doom-modeline-segment--nov-info ()
    (concat
     " " (propertize
          (cdr (assoc 'creator nov-metadata))
          'face
          'doom-modeline-project-parent-dir)
     " " (cdr (assoc 'title nov-metadata))
     " " (propertize
          (format "%d/%d" (1+ nov-documents-index) (length nov-documents))
          'face
          'doom-modeline-info)))
  (advice-add 'nov-render-title :override #'ignore)
  (defun +nov-mode-setup ()
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 80
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)
    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)

    (setq-local mode-line-format
                `((:eval (doom-modeline-segment--workspace-name))
                  (:eval (doom-modeline-segment--window-number))
                  (:eval (doom-modeline-segment--nov-info))
                  ,(propertize " "
                               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                               'display `((space :align-to
                                                 (-
                                                  (+ right right-fringe right-margin)
                                                  ,(* (let ((width (doom-modeline --font-width)))
                                                        (or (and (= width 1) 1)
                                                            (/ width (frame-char-width) 1.0)))
                                                      (string-width
                                                       (format-mode-line
                                                        (cons ""
                                                              '(:eval (doom-modeline-segment--major-mode))))))))))
                  (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))

;; Add useful data to the mode line.
(setq display-time-day-and-date t)
(display-time-mode 1)
(unless (string-match-p "^Power N/A" (battery)) (display-battery-mode 1))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8,
so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq
                            (plist-get (coding-system-plist buffer-file-coding-system) :category)
                            '(coding-category-utf-8))
                           (not
                            (memq
                             (coding-system-eol-type buffer-file-coding-system)
                             '(1 2)))) t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;;; Org mode bindings
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

(after! org
  (setq org-default-notes-file (concat org-directory "/inbox.org")))

(setq org-roam-directory "~/roam")
(setq org-roam-v2-ack t)

;; Agenda
;; (setq org-agenda-files (list "~/org/work.org"
;;                              "~/org/todo.org"))
(setq org-archive-location "archive/%s_archive::")

(setq org-bable-clojure-backend 'cider)

(setq org-default-extension ".org")
(defun org-open-org-file (file)
  "Opens an org file in the default org folder.
if no org extension is given then it will be automatically appended."
  (interactive
   (list (directory-file-name
          (read-file-name "Choose org file:" org-directory))))

  ; Check for if the file:
  ; * Already exists (and is not a directory)
  ; * Has an org extension
  ; If neither of these cases is valid then automatically append an org extension
  ; to the provided file name.
  (let ((input-file-extension (concat "." (file-name-extension file)))
        (valid-org-extension-regex-list
         (mapcar 'car
                 (seq-filter
                  (lambda (mode-pairs) (eq 'org-mode (cdr mode-pairs)))
                  auto-mode-alist))))
    (unless
        (or (and (file-exists-p file)
                 (not (file-directory-p file)))
            (and input-file-extension
                 ; check the input
                 (eval (cons 'or (mapcar (lambda (extension-regex)
                           (string-match-p extension-regex input-file-extension))
                         valid-org-extension-regex-list)))))
      (setq file (concat file org-default-extension)))) ; Otherwise set file to have an org extension)

    ; If, after the above checks, the file name still points to a directory, then
    ; throw an error since it can't be opened at that point.
    (if (file-directory-p file)
        (error "The provided file is a directory %s" file)
      (find-file file)
      (org-mode)))

(map! :leader
      :desc "Find org file"
      "f o" #'org-open-org-file)


;; Use keybinding g b to "go back" to previous location when a link is followed.
;; Use keybinding g m to "go mark" the current location so it can be returned to later.
(map! :after org
    :map org-mode-map
    :n  "g m" #'org-mark-ring-push
    :n  "g b" #'org-mark-ring-goto
    :nv "g j" #'evil-next-visual-line
    :nv "g k" #'evil-previous-visual-line
    :nv "g J" #'org-forward-element
    :nv "g K" #'org-backward-element)

;; Pomodoro
(after! org
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15)

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

(defun get-current-timestamp ()
  "returns a string timestamp in the format [yyyy-mm-dd day hh:mm]"
  (let ((time (current-time))
         (fmt (cdr org-time-stamp-formats)))
    (setq fmt (concat "[" (substring fmt 1 -1) "]"))
    (format-time-string fmt time)))

(defun worry-template ()
  "Produces a template that is meant to"
  (let ((topic (read-string "My goal is to understand why I'm worried about: ")))
    (concat
     "* " topic "\n"
     "** Intake\nDate/time: " (get-current-timestamp) "\n"
     "My goal is to understand why I'm worried about " topic ".\n"
     "** Investigation\n"
     "*** Triggers\n"
     "Pick the triggers below that apply to this issue.\n"
     "- [ ] Trying to make everyone happy\n"
     "- [ ] Perfectionism\n"
     "- [ ] I'm-not-doing-my-job panic attack\n"
     "- [ ] Imposter syndrome\n"
     "- [ ] Compare and despair\n"
     "- [ ] Other (add to master list if needed)\n\n"
     "This worry involves:\n"
     "- [ ] someone else's perception of me.\n"
     "- [ ] my perception of myself.\n\n"
     "Real world events I'm reacting to, and duration (secs/mins/hrs/etc.):\n\n"
     "Am I doing something that feels too difficult?\n\n"
     "*** Five whys\n"
     "- Q1) Why am I worried about ...\n"
     "- Q2) Why ...\n"
     "- Q3) Why ...\n"
     "- Q4) Why ...\n"
     "- Q5) Why ...\n\n"
     "** Resolution\n"
     "1. Do I /want/ to take action? If so, what is my goal? _____\n"
     "2. Do I /have/ to take action? If so, what is my goal? _____\n"
     "3. What happens if I do nothing? _____\n"
     "4. What is the worst thing I can imagine happening? _____\n"
     "5. What do I think is the most likely to happen? _____\n"
     "6. Is there an opportunity to Think Big here? What would it look like if this issue were resolved far better than I ever imagined? _____\n"
     "7. The next concrete step I will take is _____\n"
     "8. Is there a step I can add to a personal checklist to reduce the chance this worry happens again? _____\n")))

(after! org
  (add-to-list 'org-capture-templates
                '("l" "Test Capture" checkitem (file+olp+datetree org-default-notes-file) "[ ]"))
  (add-to-list 'org-capture-templates
                '("w" "Worry Capture" entry (file "worries.org") (function worry-template) :prepend t :immediate-finish t)))

;;
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

;; Configuration settings for Emacs evil mode

; Turn off case-insensitive search. Can still make
; search case-insensitive with "\c"
; (setq case-fold-search nil)

;; File for setting values related to evil-snipe mode

; There can be problems between snipe mode and magit mode.
(remove-hook! (doom-first-input) 'evil-snipe-mode)
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

(setq evil-snipe-scope 'whole-visible
      evil-snipe-repeat-scope 'whole-visible)

(defun toggle-and-activate-evil-snipe-mode ()
  "Toggles evil-snipe-mode on and off then activates the
mode map since otherwise it requires forcing the normal mode state to be activated."
  (interactive)
  (evil-snipe-local-mode)
  (evil-force-normal-state))

(map! :leader
      :desc "Evil snipe mode"
      "t S" #'toggle-and-activate-evil-snipe-mode)

(setq eshell-aliases-file "~/.doom.d/.eshell-aliases")
