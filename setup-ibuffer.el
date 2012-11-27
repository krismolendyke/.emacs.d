(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (setq ibuffer-truncate-lines t)))

(provide 'setup-ibuffer)
