;; -*- mode: elisp -*-

(defun calculate-path (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.

** From http://ergoemacs.org/emacs/organize_your_dot_emacs.html **"
  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path)
)

(load (calculate-path "package_initialize.el"))

(load (calculate-path "environment_initialize.el"))

(load (calculate-path "evil-mode_initialize.el"))

(load (calculate-path "org-mode_initialize.el"))

;; make emacs full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))


