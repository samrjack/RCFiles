;; Evil mode settings
(evil-mode 1)

(setq evil-want-C-u-scroll t)

;; Makes it so tab is treated as org-tab
(setq evil-want-C-i-jump nil)

;; Use JK as <esc>
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)

; Must come at end
(require 'evil)
