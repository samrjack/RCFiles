;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Samuel Jackson"
      user-mail-address (concat "dsiq3g" "@" "gmail.com"))

(defun local/chinese-text-support ()
  "Turn on modes to support chinese text in the buffer. May cause other text to change characteristics as well."
  (interactive)
  (variable-pitch-mode))

(setq custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file) (load custom-file))

(setq doom-theme (if (display-graphic-p)'doom-one 'doom-dracula))

(setq ns-function-modifier 'hyper)

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

(setq scroll-margin 2)

(map! :leader
      :desc "Previous mark location"
      :n "P" #'better-jumper-jump-backwards)

(setq initial-major-mode #'lisp-interaction-mode)

(setq initial-scratch-message "\
;; Welcome to the scratch buffer.

")

(defun local/dired-turn-off-file-info ()
  "Turns off the file info in dired mode"
  (interactive)
  (dired-hide-details-mode t))
(add-hook! 'dired-mode-hook #'local/dired-turn-off-file-info)

(setq whitespace-style '(trailing tabs tab-mark))

(setq-default line-spacing 0.15)

(map! :leader
      :desc "Turn on debug mode"
      "t d" #'toggle-debug-on-error)

(setq fill-column 110)

(defun local/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun local/execute-at-end-of-line (func)
  "Takes in a function then executes it at the end of the current line."
  (save-excursion (end-of-line) (funcall func)))

(defun local/evil-toggle-fold-eol ()
  "Run evil-toggle-fold at the end of the line."
  (interactive)
  (local/execute-at-end-of-line #'evil-toggle-fold))

(defun local/evil-open-fold-eol ()
  "Run evil-open-fold at the end of the line."
  (interactive)
  (local/execute-at-end-of-line #'evil-open-fold))

(defun local/evil-open-fold-rec-eol ()
  "Run evil-open-fold-rec at the end of the line."
  (interactive)
  (local/execute-at-end-of-line #'evil-open-fold-rec))

(defun local/evil-close-fold-eol ()
  "Run evil-close-fold at the end of the line."
  (interactive)
  (local/execute-at-end-of-line #'evil-close-fold))

(map! :desc "toggle fold"
      :nm "za" #'local/evil-toggle-fold-eol
      :desc "close fold"
      :nm "zc" #'local/evil-close-fold-eol
      :desc "open fold"
      :nm "zo" #'local/evil-open-fold-eol
      :desc "open fold rec"
      :nm "zO" #'local/evil-open-fold-rec-eol)

(map! :nm [tab] #'local/evil-toggle-fold-eol)

(map! :n "C-n" #'dired-sidebar-toggle-sidebar)
(map! :n "M-n" #'+treemacs/toggle)

(map! :map 'treemacs-mode-map
      :ng "M-n" #'+treemacs/toggle
      :ng "C-n" #'+treemacs/toggle)

(setq treemacs-collapse-dirs 5)

(treemacs-project-follow-mode 1)

(lsp-treemacs-sync-mode 1)

(after! projectile
  (setq projectile-track-known-projects-automatically nil))

(setq persp-sort 'created)

(setq tab-bar-show t)
(setq tab-bar-tab-name-function #'tab-bar-tab-name-current)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
(set-face-attribute 'tab-bar nil :inherit 'tab-bar-tab :foreground nil :background nil)
(map! :n "M->" #'tab-next
      :n "M-<" #'tab-previous)

(defun local/tab-bar-format-menu-bar-lambda ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  '((menu-bar menu-item (propertize " λ" 'face 'doom-modeline-evil-emacs-state)
     tab-bar-menu-bar :help "Menu Bar")))
(add-to-list 'tab-bar-format #'local/tab-bar-format-menu-bar-lambda)

(which-key-add-key-based-replacements "C-x t" "tabs")

(map! :leader :desc "Tabs" "T" tab-prefix-map)

(setq zoom-window-use-persp t)
(setq zoom-window-mode-line-color "DarkGreen")
(add-hook 'doom-load-theme-hook #'zoom-window-setup)
(zoom-window-setup)

(map! :leader
      :desc "Zoom window"
      "z" #'zoom-window-zoom)

;; Makes it so movement keys stop at camlecase sub words.
(global-subword-mode 1)

;; Make searches case sensitive
(setq-default case-fold-search nil)

(map! :leader
      :desc "Centered cursor"
      "t C" #'centered-cursor-mode)

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

(defun local/toggle-and-activate-evil-snipe-mode ()
  "Toggles evil-snipe-mode on and off then activates the
mode map since otherwise it requires forcing the normal mode state to be activated."
  (interactive)
  (evil-snipe-local-mode)
  (evil-force-normal-state))

(map! :leader
      :desc "Evil snipe mode"
      "t S" #'local/toggle-and-activate-evil-snipe-mode)

(setq local/snippet-dir (concat doom-user-dir "snippets/"))
(add-to-list 'yas-snippet-dirs 'local/snippet-dir)

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

(use-package! spray
  :commands spray-mode
  :config
  (setq spray-wpm 600
        spray-height 800)
  (defun local/spray-mode-hide-cursor()
    "Hide or unhide the cursor as is appropriate."
    (if spray-mode
        (setq-local spray--last-evil-cursor-state evil-normal-state-cursor
                    evil-normal-state-cursor '(nil))
      (setq-local evil-normal-state-cursor spray--last-evil-cursor-state)))
  (add-hook 'spray-mode-hook #'local/spray-mode-hode-cursor)
  (map! :map spray-mode-map
        "<return>" #'spray-start/stop
        "f" #'spray-faster
        "s" #'spray-slower
        "t" #'spray-time
        "<right>" #'spray-forward-word
        "h" #'spray-forward-word
        "<left>" #'spray-backward-word
        "l" #'spray-backward-word
        "q" #'spray-quit))

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
  (defun local/gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string "\n+" "\n"
                               (mapconcat (lambda (c) (if (listp (cdr c))) (cadr c)))
                               'utf-8
                               "/tmp/doom-color-theme")))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'local/gif-screencast-write-colormap))

(require 'mode-minder nil 'noerror)

(setq eshell-aliases-file "~/.doom.d/eshell/eshell-aliases")

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; Add useful data to the mode line.
(setq display-time-day-and-date t)
(display-time-mode 1)

(use-package! battery :config

    (defun local/battery-p ()
        "returns t if a battery is present for the system and nil if one is not."
        (and battery-status-function
             battery-echo-area-format
             (string-match-p "^Power N/A"
                             (battery-format
                                     battery-echo-area-format
                                     (funcall battery-status-function)))
             t))

    (unless (local/battery-p) (display-battery-mode 1))

)

(defun local/doom-modeline-conditional-buffer-encoding ()
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
(add-hook 'after-change-major-mode-hook #'local/doom-modeline-conditional-buffer-encoding)

(setq web-mode-script-padding standard-indent)
(setq web-mode-style-padding standard-indent)
(setq web-mode-block-padding standard-indent)
(setq web-mode-part-padding standard-indent)

(setq org-directory (file-name-concat "~" "org"))
;; (setq org-work-directory "~/work-org")
(setq org-work-directory (file-name-concat org-directory "work"))
(setq org-archive-location "archive/%s_archive::")

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

(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>"    #'org-backward-heading-same-level
      :n "g <down>"  #'org-forward-heading-same-level
      :n "g <left>"  #'org-up-element
      :n "g <right>" #'org-down-element)

(setq org-default-extension ".org")
(defun local/org-open-org-file (file)
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
      (setq file (concat file org-default-extension)))) ; Otherwise set file to have an org extension

  ; If, after the above checks, the file name still points to a directory, then
  ; throw an error since it can't be opened at that point.
  (if (file-directory-p file)
      (error "The provided file is a directory %s" file)
    (find-file file)
    (org-mode)))

(defun local/prompt-org-file (&optional dir default-name)
  "Prompts the user for a file inside the specified directory. Uses defualt name when no entry is given if the name is provided."
  (unless dir (setq dir org-directory))
  (directory-file-name (read-file-name "Choose org file: " dir default-name)))

(defun local/org-open-file ()
  "Prompts and opens a file in the default org directory."
  (interactive)
  (local/org-open-org-file (local/prompt-org-file org-directory "notes.org")))

(defun local/open-work-org-file (directory default-file)
  "A condensing function for opening an org directory for work purposes"
  ; Define the destination directory. Currently is hardcoded to the work dir in the org dir.
  (let ((dest-dir (file-name-concat org-work-directory directory)))
    ; First create the directory if it doesn't already exist
    (unless (file-directory-p dest-dir)
      (if (y-or-n-p (concat "directory '" dest-dir "' is not found. Create? "))
          (make-directory dest-dir 'parents)
        (message "No directory created")))
    ; Only prompt for file if the directory exists
    (when (file-directory-p dest-dir)
        (local/org-open-org-file (local/prompt-org-file dest-dir default-file)))))

(defun local/org-open-work-note ()
  "Prompts and opens a file in the org work notes directory."
  (interactive)
  (local/open-work-org-file "notes" "notes.org"))

(defun local/org-open-work-meeting ()
  "Prompts and opens a file in the org work meeting directory."
  (interactive)
  (local/open-work-org-file "meetings" "meeting.org"))

(defun local/org-open-project-note ()
  "Prompts and opens a file in the org work notes directory."
  (interactive)
  (local/open-work-org-file "projects" "project.org"))

(defun local/org-open-work-task ()
  "Opens the todo task file."
  (interactive)
  (let ((todo-file-name (file-name-concat org-work-directory "todo.org")))
    (local/org-open-org-file todo-file-name)))

(defun local/org-open-work-unfiled-notes ()
  "Opens the todo task file."
  (interactive)
  (let ((note-file-name (file-name-concat org-work-directory "notes.org")))
    (local/org-open-org-file note-file-name)))

(defun local/org-open-work-wiki ()
  "Prompts and opens a file in the org wiki tasks directory."
  (interactive)
  (local/open-work-org-file "wiki" "toSort.org"))

(map! :leader
      (:prefix ("f o" . "Org files")
       :desc "Org file" "o" #'local/org-open-file
       (:prefix ("w" . "Work")
        :desc "Meetings" "m" #'local/org-open-work-meeting
        :desc "Notes" "n" #'local/org-open-work-note
        :desc "Projects" "p" #'local/org-open-project-note
        :desc "Tasks" "t" #'local/org-open-work-task
        :desc "Unfiled Notes" "u" #'local/org-open-work-unfiled-notes
        :desc "Wiki" "w" #'local/org-open-work-wiki)))

(setq org-bable-clojure-backend 'cider)

(setq org-roam-directory "~/roam")
(setq org-roam-v2-ack t)

(use-package! websocket
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun local/org-roam-toggle-ui-xwidget ()
  "Shows the org roam ui using emacs x-widgets so you may view it in emacs instead of needing an external browser."
  (interactive)
  (let* ((host (concat "localhost:" (number-to-string org-roam-ui-port)))
         (url (concat "http://" host))
         (buf (or (xwidget-webkit-get-url-buffer host)
                  (xwidget-webkit-url-get-create url "*org-roam-ui*"))))
    (if-let ((window (get-buffer-window buf)))
        (delete-window window)
      (switch-to-buffer-other-window buf))))

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(defvar local/org-time-map (make-sparse-keymap))
;; C-t normally creates new workspaces. I'd perfer immediate access to timers.
(map! :desc "timers/clocks"
      :n "C-t" local/org-time-map)

(map! :map local/org-time-map
      ;; Clock commands
      :desc "Check-in clock"           "i" #'org-clock-in
      :desc "Check-out clock"          "o" #'org-clock-out
      :desc "Quit clock"               "q" #'org-clock-cancel
      :desc "Goto clock item"          "g" #'org-clock-goto
      :desc "Effort estimate"          "E" #'org-clock-modify-effort-estimate
      :desc "Effort estimate at point" "e" #'org-set-effort
      :desc "Display clock"            "d" #'org-clock-display

      ;; timer commands
      :desc "Start timer"            "s" #'org-timer-start
      :desc "Start timer"            "0" #'org-timer-start

      :desc "Pause/Play timer"       "p" #'org-timer-pause-or-continue
      :desc "Pause/Play timer"       "," #'org-timer-pause-or-continue

      :desc "Stop timer"             "x" #'org-timer-stop
      :desc "Stop timer"             "_" #'org-timer-stop

      :desc "Countdown timer"        ";" #'org-timer-set-timer
      :desc "Insert timer timestamp" "." #'org-timer
      :desc "Insert timer list item" "-" #'org-timer-item
)

(setq org-agenda-files (list org-directory
                             org-work-directory
                             (file-name-concat org-work-directory "meetings")
                             (file-name-concat org-work-directory "notes")
                             (file-name-concat org-work-directory "projects")
                             (file-name-concat org-work-directory "retros")
                             (file-name-concat org-work-directory "retros")
))

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

(defun local/org-present-start ()
  "Turns on settings I use during an org presentation"
  ;; Tweak font sizes
  (setq-local local/pre-org-present-face-alist face-remapping-alist)
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

(defun local/org-present-end ()
  "Turns off settings I use during an org presentation"
  ;; Reset font mapping to normal level.
  ;; (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq-local face-remapping-alist local/pre-org-present-face-alist)

  ;; Stop centering and wrapping the text
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
)

(add-hook! 'org-present-mode-hook #'local/org-present-start)
(add-hook! 'org-present-mode-quit-hook #'local/org-present-end)

(setq visual-fill-column-width 110)
(setq visual-fill-column-center-text t)

; Set default file for newly captured notes
(after! org (setq org-default-notes-file (concat org-directory "/inbox.org")))

(defun local/load-directory (dir)
  "Loads all .el files from a provided directory. If the directory doesn't exist, the function loads nothing."
  (interactive)
  (if (not (file-directory-p dir))
      (message "No directory named %s, no scripts loaded." dir)
    (let* ((load-it (lambda (f)
                      (load-file (concat (file-name-as-directory dir) f)))
                    ))
      (mapc load-it (directory-files dir nil "\\.el$")))))

(local/load-directory (concat (file-name-as-directory org-directory) "capture-templates"))

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(after! tree-sitter
  (defvar local/tree-sitter-map (make-sparse-keymap))
  (map! :map local/tree-sitter-map
        :desc "Debug mode"
        "d" #'tree-sitter-debug-mode
        :desc "TS folding"
        "f" #'ts-fold-mode
        :desc "Folding indicators"
        "i" #'ts-fold-indicators-mode
        :desc "Query builder"
        "q" #'tree-sitter-query-builder
        :desc "Highlight mode"
        "h" #'tree-sitter-hl-mode)

  (map! :map doom-leader-code-map
        :desc "Tree-sitter"
        "T" local/tree-sitter-map))

(after! ts-fold
  (defun local/update-ts-fold-definitions (mode rules)
    "Update the provided MODE with the new set of folding RULES.
MODE should be a programming mode such as go-mode.
RULES should be a list of folding rules in the format of (ts-element . folding-function)"
    (setf (alist-get mode ts-fold-range-alist) rules)
    (setq ts-fold-foldable-node-alist
          (let (alist)
            (dolist (item ts-fold-range-alist)
              (let ((mode (car item))
                    nodes)
                (dolist (rule (cdr item))
                  (push (car rule) nodes))
                (push (cons mode nodes) alist)))
            alist))))

(defun local/ts-fold-range-multi-line-seq (node offset)
  "Return the fold range in a sequence when the NODE exists over multiple lines."
  (let ((beg (1+ (tsc-node-start-position node)))
        (end (1- (tsc-node-end-position node))))
    (if (save-excursion
          (goto-char beg)
          (search-forward "\n" end t))
        (ts-fold--cons-add (cons beg end) offset)
      nil)))

(defun local/ts-fold-go-const-seq (node offset)
  "Return the fold range in sequence starting from NODE with the specific considerations of the golang const block in mind. "
  (let ((beg (+ 7 (tsc-node-start-position node)))
        (end (1- (tsc-node-end-position node))))
    (ts-fold--cons-add (cons beg end) offset)))

(setq local/ts-fold-parsers-go-list
      '((block . ts-fold-range-seq)
        (comment . ts-fold-range-seq)
        (method_spec_list . ts-fold-range-seq)
        (import_spec_list . ts-fold-range-seq)
        (field_declaration_list . ts-fold-range-seq)
        ;; (parameter_list . local/ts-fold-range-multi-line-seq)
        (literal_value . ts-fold-range-seq)
        (const_declaration . local/ts-fold-go-const-seq)))

(after! ts-fold
  (local/update-ts-fold-definitions 'go-mode local/ts-fold-parsers-go-list))

(add-hook! 'go-mode-hook #'ts-fold-indicators-mode)

(setq local/ts-fold-parsers-javascript-list
      '((object . ts-fold-range-seq)
        (array . ts-fold-range-seq)
        (export_clause . ts-fold-range-seq)
        (statement_block . ts-fold-range-seq)
        (comment . ts-fold-range-c-like-comment)))

(after! ts-fold
  (local/update-ts-fold-definitions 'javascript-mode local/ts-fold-parsers-javascript-list)
  (local/update-ts-fold-definitions 'rjsx-mode local/ts-fold-parsers-javascript-list)
  (local/update-ts-fold-definitions 'js-mode local/ts-fold-parsers-javascript-list)
  (local/update-ts-fold-definitions 'js2-mode local/ts-fold-parsers-javascript-list)
  (local/update-ts-fold-definitions 'js3-mode local/ts-fold-parsers-javascript-list)
  (local/update-ts-fold-definitions 'js3-mode local/ts-fold-parsers-javascript-list))

(after! lsp-mode
  (defvar local/lsp-mode-keymap (make-sparse-keymap))
  (map! :map local/lsp-mode-keymap
        "d" #'lsp-find-definition
        "i" #'lsp-find-implementation
        "r" #'lsp-find-references
        "R" #'lsp-rename
        "t" #'lsp-find-type-definition)

  (defun local/add-lsp-keymaps ()
    "Adds prefix keybindings for lsp keymaps."
    (interactive)
    (map! :leader
          :desc "LSP"
          "l" local/lsp-mode-keymap
          "L" lsp-mode-map))

  (add-hook! lsp-mode-hook #'local/add-lsp-keymaps))

(use-package! nov ; Novel reading
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)
  (defun local/doom-modeline-segment--nov-info ()
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
  (defun local/+nov-mode-setup ()
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
                  (:eval (local/doom-modeline-segment--nov-info))
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

  (add-hook 'nov-mode-hook #'local/+nov-mode-setup))

(defvar personal-functions-map (make-sparse-keymap))

(map!
  :leader
  :desc "Additional"
  "A" personal-functions-map)

(defun local/toggle-line-spacing ()
  "Togges between no line spacing and reasonable line spacing"
  (interactive)
  (if (null line-spacing)
      (setq line-spacing 0.15)
    (setq line-spacing nil))
  (redraw-frame (selected-frame)))

(defun local/change-line-spacing (SPACING)
  "Change the vertical spacing between lines to give more room for eyes to read"
  (interactive "NRequested spacing? ") ; 'N' uses the prefix argument if present and otherwise prompts
  (if (not SPACING)
      (local/toggle-line-spacing)
    (setq line-spacing SPACING))
  (redraw-frame (selected-frame)))

(map! :leader
      :desc "Toggle line spacing"
      "t L" #'local/toggle-line-spacing)

(map! :map personal-functions-map
      :desc "Change line spacing"
      "l" #'local/change-line-spacing)

(defun local/print-point-position ()
  "Print the position of point to the message console."
  (interactive)
  (message (number-to-string (point))))

(map! :map personal-functions-map
      :desc "Point's position"
      "p" #'local/print-point-position)
;;        (:prefix-map ("a" . "test2")
;;         (:prefix ("a" . "test")
;;          :desc "a test function to see if this works" "j" #'org-journal-new-entry
         ;; :desc "Search journal entry" "s" #'org-journal-search))))
;;

(defun local/scratch (&optional BUFNUM)
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
      "s" #'local/scratch)

(defun local/remove-all-overlays ()
  "Removes all overlays in the file."
  (interactive)
  (remove-overlays))

(map! :map personal-functions-map
      :desc "Remove overlays"
      "O" #'local/remove-all-overlays)

(defun local/set-fill-column-val (col)
  "Set the fill-column value to COL"
  ;; (interactive (concat"nNew fill-column value (" (number-to-string fill-column) "): "))
  (interactive "nNew fill-column value: ")
  (setq fill-column col))

(map! :map personal-functions-map
      :desc "set fill-column"
      "c" #'local/set-fill-column-val)

(remove-hook! '(magit-mode-hook find-file-hook) #'forge-bug-reference-setup)

;; (after! evil (evil-select-search-module 'evil-search-module 'isearch))
