(require 'autopair)
(require 'python)

(add-hook 'python-mode-hook
          #'(lambda ()
              (toggle-truncate-lines 1)
              (linum-mode 1)
              (setq fill-column 118
                    autopair-handle-action-fns (list #'autopair-default-handle-action
                                                     #'autopair-python-triple-quote-action))))

(provide 'setup-python)
