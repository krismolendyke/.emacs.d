(setq-default ispell-program-name "/usr/local/bin/aspell")

;; When to turn on flyspell-mode.
(dolist (hook '(text-mode-hook))
  (add-hook hook #'(lambda () (flyspell-mode 1))))

;; When to turn on flyspell-prog-mode for comments and strings in source.
;; (dolist (hook '(emacs-lisp-mode-hook
;;                 lisp-mode-hook))
;;   (add-hook hook #'(lambda () (flyspell-prog-mode))))

;; Do not emit to *Messages*.
(setq flyspell-issue-message-flag nil)

(provide 'setup-flyspell)
