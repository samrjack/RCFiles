;; Make Org mode work with files ending in .org
(add-to-list 'load-path (expand-file-name "~/org/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(add-hook 'org-mode-hook '(lambda () (visual-line-mode 1)))

;; Setting up export
(setenv "PATH" (concat (getenv "path") ":/Library/TeX/texbin/"))
;; Adds pretty blue links to export instead of default black links.
(setq org-latex-packages-alist '("\\hypersetup{colorlinks=true,linkcolor=blue}"))

;; Org mode options
(setq org-special-ctrl-a/e t)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)

;; Enable Org mode
(require 'org)

;;
;; directory and special file settings
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "refile.org"))

;;
;; Global key bindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)
