(require 'org-install)

(setq org-directory (expand-file-name "org" google-drive-directory))
(setq org-log-done t)

(setq org-agenda-files
      (list
       (expand-file-name "org/k20e.org" google-drive-directory)
       (expand-file-name "org/monetate.org" google-drive-directory)))

(provide 'setup-org-mode)
