(autoload 'paredit-mode "paredit" nil t)

;; When to turn on paredit.
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                cider-repl-mode-hook))
  (add-hook hook #'(lambda nil (paredit-mode 1))))

;; Map close-parenthesis.
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [?\)] 'paredit-close-parenthesis)
     (define-key paredit-mode-map [(meta ?\))]
                 'paredit-close-parenthesis-and-newline)))

(provide 'setup-paredit)
