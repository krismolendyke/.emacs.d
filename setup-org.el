(require 'org)
(require 'org-install)

;; Set the org directory.
(setq org-directory (expand-file-name "org" google-drive-directory))

;; Automatically insert a timestamp when a task is marked DONE.
(setq org-log-done t)

;; org-capture.
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;; Agenda files.
(setq org-agenda-files
      (list (expand-file-name "k20e.org" org-directory)
            (expand-file-name "work.org" org-directory)))

(provide 'setup-org)
