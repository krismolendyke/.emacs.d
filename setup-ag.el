(require 'ag)

(setq ag-arguments
      '( "--color" "--smart-case" "--nogroup" "--column" "--smart-case" "--stats" "--"))

(global-set-key (kbd "C-x C-a") 'ag-project)

(provide 'setup-ag)
