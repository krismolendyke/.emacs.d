(require 'ag)

(setq ag-arguments
      '( "--color" "--smart-case" "--nogroup" "--column" "--smart-case" "--stats" "--")
      ag-highlight-search t)

(global-set-key (kbd "C-x C-a") 'ag-project)

(provide 'setup-ag)
