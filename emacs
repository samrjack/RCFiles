;; -*- mode: elisp -*-

;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Standard packages to install
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
  
;; Org bullet points look nice
(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil
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



;; Allow for flexable matching when moving buffers
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer-other-window)


;; Evil mode settings
(setq evil-want-C-u-scroll t)

  ; Must come at end
(require 'evil)
(evil-mode 1)

;; END EVIL MODE SETTINGS

;; Move to more logical location later
;; (setq inhibit-startup-message t)

;; Enable transient mark mode
(transient-mark-mode 1)

;;Org mode configuration
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.og$" . org-mode))
;; The above is the default in recent emacs
(setq backup-directory-alist `(("." . "~/.swap")))
(setq backup-by-copying t)

;; Enable Org mode
(require 'org)

;; END ORGMODE


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (ace-window ace-wiindow org-bullets 2048-game mines minesweeper steam sudoku suggest threes which-key try ereader eshell-git-prompt eshell-prompt-extras eshell-up use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.


'(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
