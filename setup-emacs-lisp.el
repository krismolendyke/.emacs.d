(require 'autopair)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (electric-indent-mode)
            ;; autopair `' when writing comments or strings.
            (push '(?` . ?')
                  (getf autopair-extra-pairs :comment))
            (push '(?` . ?')
                  (getf autopair-extra-pairs :string))))

(provide 'setup-emacs-lisp)
