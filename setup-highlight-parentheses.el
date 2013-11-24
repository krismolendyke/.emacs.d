(require 'highlight-parentheses)

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                cider-repl-mode-hook))
  (add-hook hook #'(lambda ()
                     (highlight-parentheses-mode))))

(provide 'setup-highlight-parentheses)
