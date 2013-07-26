(require 'monetate-mode)

(dolist (hook '(python-mode-hook js-mode-hook))
  (add-hook hook
            (lambda ()
              (if (monetate-repo-p)
                  (monetate-mode 1)))))

(provide 'setup-monetate)
