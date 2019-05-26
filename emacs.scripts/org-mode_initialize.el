;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.og$" . org-mode))

(add-hook 'org-mode-hook '(lambda () (visual-line-mode 1)))

;; Add blue links instead of black links
(setq org-latex-packages-alist '("\\hypersetup{colorlinks=true,linkcolor=blue}"))

;; Enable Org mode
(require 'org)
