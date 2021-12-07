;; Values that affect the global emacs experience

;; Allow for flexable matching when moving buffers
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Change the buffers menue to use an enhansed UI
(defalias 'list-buffers 'ibuffer-other-window)

;; Changes window selection text to be prettier
(custom-set-faces '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;; Enable transient mark mode so text is highlighted.
(transient-mark-mode 1)

;; Move all backups to a single directory.
(make-directory "~/.swap" t)
(setq backup-directory-alist `(("." . "~/.swap")))
(setq backup-by-copying t)

;; Disable defaults
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;;
(setq lisp-body-indent 4)
