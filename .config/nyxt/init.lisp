(in-package #:nyxt-user)

;; (dolist (file (list ())))
(print (expand-path *init-file-path*))

(define-configuration buffer
    ((default-modes (append '(emacs-mode) %slot-default%))))
