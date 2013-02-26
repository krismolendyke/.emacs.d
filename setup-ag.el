(require 'ag)

(setq ag-arguments
      '( "--color" "--smart-case" "--nogroup" "--column" "--smart-case" "--stats" "--"))

(add-hook 'ag-mode-hook (lambda ()
                          (setq truncate-lines t)))

(provide 'setup-ag)
