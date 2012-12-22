(add-hook 'python-mode-hook
          #'(lambda ()
              (toggle-truncate-lines 1)
              (setq fill-column 118)
              (linum-mode 1)))

(provide 'setup-python)
