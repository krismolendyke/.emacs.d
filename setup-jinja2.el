(require 'jinja2-mode)

;; There is sadly no Emacs Soy Template mode... yet.
(setq auto-mode-alist
      (cons '("\\.soy" . jinja2-mode) auto-mode-alist))

(provide 'setup-jinja2)
