;; flyspell.
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; When to turn on flyspell.
(dolist (hook '(org-mode-hook
                text-mode-hook))
  (add-hook hook #'(lambda () (flyspell-mode 1))))

(provide 'setup-flyspell)
