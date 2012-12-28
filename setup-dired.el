(add-hook 'dired-mode-hook
          #'(lambda ()
              (auto-revert-mode 1)
              (setq
               auto-revert-verbose nil
               truncate-lines 1)))

(provide 'setup-dired)
