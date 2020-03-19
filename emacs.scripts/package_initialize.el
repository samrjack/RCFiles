;; Package setup
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;; Start package installs ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Org Mode Packages ;;;;;
    (use-package org
        :ensure t)
 
    ;; Org bullet points look nice
    (use-package org-bullets
        :ensure t
        :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;;;; Evil Mode packages ;;;;;
    (use-package evil
        :ensure t)

    (use-package evil-escape
        :ensure t)

    (use-package evil-org
        :ensure t
        :after org
        :config
        (add-hook 'org-mode-hook 'evil-org-mode)
        (add-hook 'evil-org-mode-hook
              (lambda ()
                  (evil-org-set-key-theme)))
        (require 'evil-org-agenda))
	

    (use-package evil-visual-mark-mode
        :ensure t)

;;;;; Programming language environments ;;;;;
    ;; Haskell
    (use-package intero
        :ensure t)

    (use-package dante
        :ensure t)

;;;;; Packages for general environment changes ;;;;;
    ;; Allows you to try a package before installing
    (use-package try
        :ensure t)

    ;; Hints what keys can be pressed from ctrl-x or other leaders
    (use-package which-key
        :ensure t
        :config (which-key-mode))


    ;; Windows Managment
    (use-package ace-window
        :ensure t
        :config (progn
            (global-set-key [remap other-window] 'ace-window)
            (custom-set-faces '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))) ))))

    (use-package ivy
        :ensure t)

    ;; osx application stuff
    (use-package xclip
        :ensure t)
    

;;;;; Editing Features ;;;;;
    (use-package auto-complete
        :ensure t
        :init (progn
            (ac-config-default)
            (global-auto-complete-mode t)))

    (use-package rainbow-delimiters
        :ensure t
        :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
        

    ;; Themes
    (use-package dracula-theme
        :ensure t
        :config (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

    (use-package gruvbox-theme
        :ensure t
        :config (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

    (use-package professional-theme
        :ensure t
        :config (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

;;;;; Git ;;;;;
    (use-package magit
        :ensure t)

;;;;; Game related ;;;;;
    (use-package 2048-game
        :ensure t)

    (use-package mines
        :ensure t)

    (use-package minesweeper
        :ensure t)

    (use-package steam
        :ensure t)

    (use-package sudoku
        :ensure t)

    (use-package threes
        :ensure t)

    ;;;;; Others that looked interesting ;;;;;
    (use-package suggest
        :ensure t)

    (use-package ereader
        :ensure t)

    (use-package eshell-git-prompt
        :ensure t)

    (use-package eshell-prompt-extras
        :ensure t)

    (use-package eshell-up
        :ensure t)

    (use-package helm
        :ensure t)



;;; Advanced search
;(use-package swiper
;  :swiper t
;  :config (progn
;	    (ivy-mode 1)
;	    (setq ivy-use-virtual-buffers t)
;	    (setq enable-recursive-minibuffers t)
;	    (global-set-key "\C-s" 'swiper)
;	    (global-set-key (kbd "C-c C-r") 'ivy-resume)
;	    (global-set-key (kbd "<f6>") 'ivy-resume)
;	    (global-set-key (kbd "M-x") 'counsel-M-x)
;	    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;	    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;	    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;	    (global-set-key (kbd "<f1> l") 'counsel-find-library)
;	    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;	    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;	    (global-set-key (kbd "C-c g") 'counsel-git)
;	    (global-set-key (kbd "C-c j") 'counsel-git-grep)
;	    (global-set-key (kbd "C-c k") 'counsel-ag)
;	    (global-set-key (kbd "C-x l") 'counsel-locate)
;	    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;	    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))


; If a tabbar is desired. TODO try later
;(use-package tabbar
;  :ensure t
;  :config (tabbar-mode 1))
