;;; -*- lexical-binding: t; -*-
;; File for setting values related to evil-snipe mode

; There can be problems between snipe mode and magit mode.
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

(map! :map general-override-mode-map :nv "s" #'evil-substitute)
