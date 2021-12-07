;;; org-template-settings.el -*- lexical-binding: t; -*-

(defun get-current-timestamp ()
  "returns a string timestamp in the format [yyyy-mm-dd day hh:mm]"
  (let ((time (current-time))
         (fmt (cdr org-time-stamp-formats)))
    (setq fmt (concat "[" (substring fmt 1 -1) "]"))
    (format-time-string fmt time)))

(defun worry-template ()
  "Produces a template that is meant to"
  (let ((topic (read-string "My goal is to understand why I'm worried about: ")))
    (concat
     "* " topic "\n"
     "** Intake\nDate/time: " (get-current-timestamp) "\n"
     "My goal is to understand why I'm worried about " topic ".\n"
     "** Investigation\n"
     "*** Triggers\n"
     "Pick the triggers below that apply to this issue.\n"
     "- [ ] Trying to make everyone happy\n"
     "- [ ] Perfectionism\n"
     "- [ ] I'm-not-doing-my-job panic attack\n"
     "- [ ] Imposter syndrome\n"
     "- [ ] Compare and despair\n"
     "- [ ] Other (add to master list if needed)\n\n"
     "This worry involves:\n"
     "- [ ] someone else's perception of me.\n"
     "- [ ] my perception of myself.\n\n"
     "Real world events I'm reacting to, and duration (secs/mins/hrs/etc.):\n\n"
     "Am I doing something that feels too difficult?\n\n"
     "*** Five whys\n"
     "- Q1) Why am I worried about ...\n"
     "- Q2) Why ...\n"
     "- Q3) Why ...\n"
     "- Q4) Why ...\n"
     "- Q5) Why ...\n\n"
     "** Resolution\n"
     "1. Do I /want/ to take action? If so, what is my goal? _____\n"
     "2. Do I /have/ to take action? If so, what is my goal? _____\n"
     "3. What happens if I do nothing? _____\n"
     "4. What is the worst thing I can imagine happening? _____\n"
     "5. What do I think is the most likely to happen? _____\n"
     "6. Is there an opportunity to Think Big here? What would it look like if this issue were resolved far better than I ever imagined? _____\n"
     "7. The next concrete step I will take is _____\n"
     "8. Is there a step I can add to a personal checklist to reduce the chance this worry happens again? _____\n")))

(after! org
  (add-to-list 'org-capture-templates
                '("l" "Test Capture" checkitem (file+olp+datetree org-default-notes-file) "[ ]"))
  (add-to-list 'org-capture-templates
                '("w" "Worry Capture" entry (file "worries.org") (function worry-template) :prepend t :immediate-finish t)))

;;
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
