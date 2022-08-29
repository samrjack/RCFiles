;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Samuel Jackson"
      user-mail-address (concat "dsiq3g" "@" "gmail.com"))

(defun chinese-text-support ()
  "Turn on modes to support chinese text in the buffer. May cause other text to change characteristics as well."
  (interactive)
  (variable-pitch-mode))

(setq custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file) (load custom-file))

(setq doom-theme (if (display-graphic-p)'doom-one 'doom-dracula))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Let the undo buffer use up to 100Mb
(setq undo-limit 100000000)

;; Resize all windows when a new one comes in so they have
;; equal space.
(setq-default window-combination-resize t
;; changes the cursor to be the size of a gliph in the buffer.
              x-stretch-cursor t)

;; (setq-default left-margin-width 1)
;; (set-window-buffer nil (current-buffer))

;; Some paste related settings.
(setq save-interprogram-paste-before-kill t
      select-enable-clipboard nil)

(evil-define-operator evil-copy-to-clipboard (beg end &optional type _ handler)
  "Saves the characters in motion into they system clipboard through the '+' register"
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (evil-yank beg end type ?+ handler))

(evil-define-command evil-paste-from-clipboard
  (count &optional _ handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "*P<x>")
  (evil-paste-before count ?+ handler))

(map! :desc "Paste from clipboard" :nvieomg "s-v" #'evil-paste-from-clipboard
      :desc "Copy to clipboard"    :nvieomg "s-c" #'evil-copy-to-clipboard)

;; leave some space at the bottom while scrolling down so the
;; cursor isn't hugging the bottom edge.
(setq scroll-margin 2)

(setq initial-major-mode #'lisp-interactive-mode)

(setq initial-scratch-message "\
;; Welcome to the scratch buffer.

")

(defun dired-turn-off-file-info ()
  "Turns off the file info in dired mode"
  (interactive)
  (dired-hide-details-mode t))
(add-hook! 'dired-mode-hook #'dired-turn-off-file-info)

(map! :map dired-mode-map
      :leader
      :desc "toggle dired file info"
      :n "t d" #'dired-hide-details-mode)

(setq whitespace-style '(trailing tabs tab-mark))

(setq-default line-spacing 0.15)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(map! :n "TAB" #'+fold/toggle)

(map! :n "C-n" #'dired-sidebar-toggle-sidebar)
(map! :n "M-n" #'+treemacs/toggle)

(map! :map 'treemacs-mode-map
      :ng "M-n" #'+treemacs/toggle
      :ng "C-n" #'+treemacs/toggle)

(setq treemacs-collapse-dirs 5)

(after! projectile
  (setq projectile-track-known-projects-automatically nil))

(setq persp-sort 'created)

(setq tab-bar-show t)
(setq tab-bar-tab-name-function #'tab-bar-tab-name-current)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
(set-face-attribute 'tab-bar nil :inherit 'tab-bar-tab :foreground nil :background nil)
(map! :n "M->" #'tab-next
      :n "M-<" #'tab-previous)

(defun tab-bar-format-menu-bar-lambda ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  '((menu-bar menu-item (propertize " λ" 'face 'doom-modeline-evil-emacs-state)
     tab-bar-menu-bar :help "Menu Bar")))
(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar-lambda)

(which-key-add-key-based-replacements "C-x t" "tabs")

(map! :leader :desc "Tabs" "T" tab-prefix-map)

(setq zoom-window-use-persp t)

(setq zoom-window-mode-line-color "DarkGreen")
(map! :leader
      :desc "Zoom window"
      "z" #'zoom-window-zoom)

;; Makes it so movement keys stop at camlecase sub words.
(global-subword-mode 1)

;; Make searches case sensitive
(setq-default case-fold-search nil)

(map! :n "g /"   #'which-key-show-top-level
      :n "g C-/" #'which-key-show-full-major-mode
      :n "g ?"   #'which-key-show-full-major-mode
      :n "g M-/" #'which-key-show-minor-mode-keymap)
(setq which-key-idle-delay 0.5)

(marginalia-mode)

(map! :map help-map
      "b b" 'embark-bindings
      "b B" 'describe-bindings)

(remove-hook! (org-mode markdown-mode rst-mode asciidoc-mode latex-mode) #'writegood-mode)
(add-hook 'writegood-mode-hook 'writegood-passive-voice-turn-off)
(map! :leader
      :desc "Write good mode"
      "t W" #'writegood-mode)

;; Disable flycheck mode on load. Can be re-enabled in a buffer with SPC t f
(remove-hook! (doom-first-buffer) #'global-flycheck-mode)

;; Make undo revert smaller sections of text instead of all text
;; added while in insert mode.
(setq evil-want-fine-undo t)

; Remove default snipe mode
(remove-hook! (doom-first-input) 'evil-snipe-mode)
; There can be problems between snipe mode and magit mode.
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

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'info-selection-hook 'info-colors-fontify-node)

;; Set the correct dictionary for spell check.
(setq ispell-dictionary "en")

(global-origami-mode)

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))

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

(setq eshell-aliases-file "~/.doom.d/.eshell-aliases")

;; Add useful data to the mode line.
(setq display-time-day-and-date t)
(display-time-mode 1)

(use-package! battery :config

    (defun battery-p ()
        "returns t if a battery is present for the system and nil if one is not."
        (and battery-status-function
             battery-echo-area-format
             (string-match-p "^Power N/A"
                             (battery-format
                                     battery-echo-area-format
                                     (funcall battery-status-function)))
             t))

    (unless (battery-p) (display-battery-mode 1))

)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8,
so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (if (and
                       ; Checking for UTF-8
                       (memq
                        (plist-get (coding-system-plist buffer-file-coding-system) :category)
                        '(coding-category-utf-8))
                       ; Checking for LF line ending
                       (not
                        (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t nil)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq web-mode-script-padding standard-indent)
(setq web-mode-style-padding standard-indent)
(setq web-mode-block-padding standard-indent)
(setq web-mode-part-padding standard-indent)

(setq org-directory "~/org")
(setq org-archive-location "archive/%s_archive::")

(setq org-bable-clojure-backend 'cider)

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

(defun prompt-org-file (&optional dir default-name)
  "Prompts the user for a file inside the specified directory. Uses defualt name when no entry is given if the name is provided."
  (unless dir (setq dir org-directory))
  (directory-file-name (read-file-name "Choose org file: " dir default-name)))

(defun org-open-file ()
  "Prompts and opens a file in the default org directory."
  (interactive)
  (org-open-org-file (prompt-org-file org-directory "notes.org")))

(defun open-work-org-file (directory default-file)
  "A condensing function for opening an org directory for work purposes"
  (org-open-org-file (prompt-org-file (concat (file-name-as-directory org-directory) (file-name-as-directory "work") (file-name-as-directory directory)) default-file)))

(defun org-open-work-note ()
  "Prompts and opens a file in the org work notes directory."
  (interactive)
  (open-work-org-file "notes" "notes.org"))

(defun org-open-project-note ()
  "Prompts and opens a file in the org work meeting directory."
  (interactive)
  (open-work-org-file "meetings" "meeting.org"))

(defun org-open-project-note ()
  "Prompts and opens a file in the org work notes directory."
  (interactive)
  (open-work-org-file "projects" "project.org"))

(defun org-open-work-task ()
  "Prompts and opens a file in the org work tasks directory."
  (interactive)
  (open-work-org-file "tasks" "schedule.org"))

(defun org-open-work-wiki ()
  "Prompts and opens a file in the org work tasks directory."
  (interactive)
  (open-work-org-file "wiki" "toSort.org"))

(map! :leader
      (:prefix ("f o" . "Org files")
       :desc "Org file" "o" #'org-open-file
       (:prefix ("w" . "Work")
        :desc "Meetings" "m" #'org-open-work-meeting
        :desc "Notes" "n" #'org-open-work-note
        :desc "Projects" "p" #'org-open-project-note
        :desc "Tasks" "t" #'org-open-work-task
        :desc "Wiki" "w" #'org-open-work-wiki)))

(setq org-roam-directory "~/roam")
(setq org-roam-v2-ack t)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

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
    org-pomodoro-long-break-sound-p t)

)

(defun my/org-present-start ()
  "Turns on settings I use during an org presentation"
  ;; Tweak font sizes
  (setq-local my/pre-org-present-face-alist face-remapping-alist)
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Center the presentation and have line wraps
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
)

(defun my/org-present-end ()
  "Turns off settings I use during an org presentation"
  ;; Reset font mapping to normal level.
  ;; (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq-local face-remapping-alist my/pre-org-present-face-alist)

  ;; Stop centering and wrapping the text
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
)

(add-hook! 'org-present-mode-hook #'my/org-present-start)
(add-hook! 'org-present-mode-quit-hook #'my/org-present-end)

(setq visual-fill-column-width 110)
(setq visual-fill-column-center-text t)

; Set default file for newly captured notes
(after! org (setq org-default-notes-file (concat org-directory "/inbox.org")))

(defun load-directory (dir)
  "Loads all .el files from a provided directory. If the directory doesn't exist, the function loads nothing."
  (interactive)
  (if (not (file-directory-p dir))
      (message "No directory named %s, no scripts loaded." dir)
    (let* ((load-it (lambda (f)
                      (load-file (concat (file-name-as-directory dir) f)))
                    ))
      (mapc load-it (directory-files dir nil "\\.el$")))))

(load-directory (concat (file-name-as-directory org-directory) "capture-templates"))

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

(defvar personal-functions-map (make-sparse-keymap))

(map!
  :leader
  :desc "Additional"
  "A" personal-functions-map)

(defun toggle-line-spacing ()
  "Togges between no line spacing and reasonable line spacing"
  (interactive)
  (if (null line-spacing)
      (setq line-spacing 0.15)
    (setq line-spacing nil))
  (redraw-frame (selected-frame)))

(defun change-line-spacing (spacing)
  "Change the vertical spacing between lines to give more room for eyes to read"
  (interactive "NRequested spacing? ") ; 'N' uses the prefix argument if present and otherwise prompts
  (if (not spacing)
      (toggle-line-spacing)
    (setq line-spacing spacing))
  (redraw-frame (selected-frame)))

(map! :leader
      :desc "Toggle line spacing"
      "t L" #'toggle-line-spacing)

(map! :map personal-functions-map
      :desc "Change line spacing"
      "l" #'change-line-spacing)

(defun print-point-position ()
  "Print the position of point to the message console."
  (interactive)
  (message (number-to-string (point))))

(map! :map personal-functions-map
      :desc "Point's position"
      "p" #'print-point-position)
;;        (:prefix-map ("a" . "test2")
;;         (:prefix ("a" . "test")
;;          :desc "a test function to see if this works" "j" #'org-journal-new-entry
         ;; :desc "Search journal entry" "s" #'org-journal-search))))
;;

(defun scratch (&optional BUFNUM)
  "Switches to (and creates if necessary) the scratch buffer corresponding to the provided scratch buffer number. If no number was given, then it creates a new sratch buffer at the next avaliable position.
Buffer numbers start at 1 to make accessing the default buffer easier.

Buffers are labled as *scratch* through *scratchX*."
  (interactive "P")
  (let ((create-buffer-name (lambda (num)
                              (concat "*scratch"
                                      (if (= num 1)
                                          ""
                                        (int-to-string num))
                                      "*")))
        (already-open nil)
        (n 1)
        buffer-name)
    (if BUFNUM
        (setq buffer-name (funcall create-buffer-name BUFNUM))
      ;; Loops through possible buffer names until it finds one
      ;; that doesn't exist
      (while (progn
               (setq buffer-name (funcall create-buffer-name n))
               (setq n (1+ n))
               (message buffer-name)
               (get-buffer buffer-name))))

      ;; Check if the buffer already exists before switching so the
      ;; major mode doesn't get forcibly changed.
      (setq already-open (get-buffer buffer-name))
      (switch-to-buffer (get-buffer-create buffer-name))
      (unless already-open (funcall initial-major-mode)
                           (insert initial-scratch-message))))

(map! :map personal-functions-map
      :desc "scratch buffer"
      "s" #'scratch)

(remove-hook! '(magit-mode-hook find-file-hook) #'forge-bug-reference-setup)
