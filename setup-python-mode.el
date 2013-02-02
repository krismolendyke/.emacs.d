(require 'python-mode)

(add-hook 'python-mode-hook
          #'(lambda ()
              (toggle-truncate-lines 1)
              (linum-mode 1)
              (setq fill-column 118
                    autopair-handle-action-fns (list #'autopair-default-handle-action
                                                     #'autopair-python-triple-quote-action))))

(setq py-install-directory (expand-file-name "python-mode" site-lisp-directory))
(set-default 'py-shell-name "ipython")

(provide 'setup-python-mode)
