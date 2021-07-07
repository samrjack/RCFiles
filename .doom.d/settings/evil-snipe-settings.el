;;; -*- lexical-binding: t; -*-
;; File for setting values related to evil-snipe mode

; There can be problems between snipe mode and magit mode.
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

(remove-hook! 'doom-first-input-hook 'evil-snipe-mode)

(defun toggle-and-activate-evil-snipe-mode ()
  "Toggles evil-snipe-mode on and off then activates the
mode map since otherwise it requires forcing the normal mode state to be activated."
  (interactive)
  (evil-snipe-local-mode)
  (evil-force-normal-state))

(map! :leader
      :desc "Evil snipe mode"
      "t S" #'toggle-and-activate-evil-snipe-mode)
