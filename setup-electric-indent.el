(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook))
  (add-hook hook #'(lambda () (electric-indent-mode 1))))

(provide 'setup-electric-indent)
