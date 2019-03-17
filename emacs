;; (package-initialize)

;; All loading happens in init.el
(load "~/.emacs.scripts/init.el")

;; Required so emacs can add it's own values when desired.
(custom-set-variables
 '(ansi-color-faces-vector
	[default default default italic underline success warning error])

 '(ansi-color-names-vector
	["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])

 '(custom-enabled-themes (quote (deeper-blue)))
)
