;; Do not wrap lines.
(add-hook 'dired-mode-hook #'(lambda () (setq truncate-lines 1)))

(provide 'setup-dired)
