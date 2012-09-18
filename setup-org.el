(require 'org)
(require 'org-install)

;; Set the org directory.
(setq org-directory (expand-file-name "org" google-drive-directory))

;; Automatically insert a timestamp when a task is marked DONE.
(setq org-log-done t)

;; Display entities as UTF-8 characters.
(add-hook 'org-mode-hook
          #'(lambda ()
              (org-toggle-pretty-entities)
              (visual-line-mode 0)))

;; org-capture.
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;; Agenda files.
(setq org-agenda-files
      (list (expand-file-name "k20e.org" org-directory)
            (expand-file-name "work.org" org-directory)))

(provide 'setup-org)
