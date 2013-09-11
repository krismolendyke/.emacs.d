(require 'autopair)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local electric-indent-mode t)
            ;; autopair `' when writing comments or strings.
            (push '(?` . ?')
                  (getf autopair-extra-pairs :comment))
            (push '(?` . ?')
                  (getf autopair-extra-pairs :string))))

(provide 'setup-emacs-lisp)
