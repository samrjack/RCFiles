;;; -*- lexical-binding: t; -*-
;;; Org mode bindings
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-roam-directory "~/roam")
(setq org-roam-v2-ack t)

;; Agenda
;; (setq org-agenda-files (list "~/org/work.org"
;;                              "~/org/todo.org"))
(setq org-archive-location "archive/%s_archive::")

(setq org-bable-clojure-backend 'cider)

(setq org-default-extension ".org")
(defun org-open-org-file (file)
  "Opens an org file in the default org folder.
if no org extension is given then it will be automatically appended."
  (interactive
   (list (directory-file-name
          (read-file-name "Choose org file:" org-directory))))

  ; Check for if the file:
  ; * Already exists (and is not a directory)
  ; * Has an org extension
  ; If neither of these cases is valid then automatically append an org extension
  ; to the provided file name.
  (let ((input-file-extension (concat "." (file-name-extension file)))
        (valid-org-extension-regex-list
         (mapcar 'car
                 (seq-filter
                  (lambda (mode-pairs) (eq 'org-mode (cdr mode-pairs)))
                  auto-mode-alist))))
    (unless
        (or (and (file-exists-p file)
                 (not (file-directory-p file)))
            (and input-file-extension
                 ; check the input
                 (eval (cons 'or (mapcar (lambda (extension-regex)
                           (string-match-p extension-regex input-file-extension))
                         valid-org-extension-regex-list)))))
      (setq file (concat file org-default-extension)))) ; Otherwise set file to have an org extension)

    ; If, after the above checks, the file name still points to a directory, then
    ; throw an error since it can't be opened at that point.
    (if (file-directory-p file)
        (error "The provided file is a directory %s" file)
      (find-file file)
      (org-mode)))

(map! :leader
      :desc "Find org file"
      "f o" #'org-open-org-file)


;; Use keybinding g b to "go back" to previous location when a link is followed.
;; Use keybinding g m to "go mark" the current location so it can be returned to later.
(after! org
  (map! :map org-mode-map
        :n  "g m" #'org-mark-ring-push
        :n  "g b" #'org-mark-ring-goto
        :nv "g j" #'evil-next-visual-line
        :nv "g k" #'evil-previous-visual-line
        :nv "g J" #'org-forward-element
        :nv "g K" #'org-backward-element))

;; Pomodoro
(after! org
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15)

  (setq org-pomodoro-play-sounds t
        ;; org-pomodoro-start-sound-p f
        ;; org-pomodoro-ticking-sound-p f
        org-pomodoro-killed-sound-p t
        org-pomodoro-finished-sound-p t
        org-pomodoro-short-break-sound-p t
        org-pomodoro-long-break-sound-p t))

;; need to find (or make) some better alert audio files.
;; (setq ;org-pomodoro-start-sound ()
;;       ;org-pomodoro-ticking-sound ()
;;       org-pomodoro-killed-sound ()
;;       org-pomodoro-finished-sound ()
;;       org-pomodoro-short-break-sound ()
;;       org-pomodoro-long-break-sound ())

;;; config.el ends here
