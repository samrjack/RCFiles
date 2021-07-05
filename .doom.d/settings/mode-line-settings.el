;;; mode-line-settings.el -*- lexical-binding: t; -*-

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
