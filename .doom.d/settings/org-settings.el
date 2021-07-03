;;; Org mode bindings
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; Agenda
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/todo.org"))

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

(after! org
  (add-to-list 'org-capture-templates
                '("l" "Test Capture" checkitem (file+olp+datetree org-default-notes-file) "[ ]")))


;; Example of org capture templates
;; for example text, see https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el
;; (after! org
;;   (setq org-capture-templates '(
;;     ("t" "Todo" entry () "" :prepend t)
;;     ("k" "Kudos" entry () "" :prepend t)
;;     ("f" "Followup" entry () "" :prepend t)
;;     ("p" "Personal" entry () "" :prepend t)
;;     ("P" "Project" entry () "" :prepend t)
;;     ("j" "Journal" entry () "" :prepend t)
;;   )))



;; '(
;;   ("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* [ ] %?\n%i\n%a" :prepend t)
;;           ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?\n%i\n%a" :prepend t)
;;           ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
;;            "* %U %?\n%i\n%a" :prepend t)

;;           ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
;;           ;; {todo,notes,changelog}.org file is found in a parent directory.
;;           ;; Uses the basename from `+org-capture-todo-file',
;;           ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
;;           ("p" "Templates for projects")
;;           ("pt" "Project-local todo" entry  ; {project-root}/todo.org
;;            (file+headline +org-capture-project-todo-file "Inbox")
;;            "* TODO %?\n%i\n%a" :prepend t)
;;           ("pn" "Project-local notes" entry  ; {project-root}/notes.org
;;            (file+headline +org-capture-project-notes-file "Inbox")
;;            "* %U %?\n%i\n%a" :prepend t)
;;           ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
;;            (file+headline +org-capture-project-changelog-file "Unreleased")
;;            "* %U %?\n%i\n%a" :prepend t)

;;           ;; Will use {org-directory}/{+org-capture-projects-file} and store
;;           ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
;;           ;; support `:parents' to specify what headings to put them under, e.g.
;;           ;; :parents ("Projects")
;;           ("o" "Centralized templates for projects")
;;           ("ot" "Project todo" entry
;;            (function +org-capture-central-project-todo-file)
;;            "* TODO %?\n %i\n %a"
;;            :heading "Tasks"
;;            :prepend nil)
;;           ("on" "Project notes" entry
;;            (function +org-capture-central-project-notes-file)
;;            "* %U %?\n %i\n %a"
;;            :heading "Notes"
;;            :prepend t)
;;           ("oc" "Project changelog" entry
;;            (function +org-capture-central-project-changelog-file)
;;            "* %U %?\n %i\n %a"
;;            :heading "Changelog"
;;            :prepend t)))

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
