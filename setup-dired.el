(add-hook 'dired-mode-hook
          #'(lambda ()
              (auto-revert-mode 1)
              (setq
               auto-revert-verbose nil
               truncate-lines 1)
              (set-face-foreground 'dired-flagged "tomato1")
              (set-face-attribute 'dired-flagged nil :strike-through t)))

(provide 'setup-dired)
