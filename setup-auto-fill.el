;; When to turn on auto-fill and set fill-column to a reasonable value.  This
;; would probably be better dealt with by a data structure that maps mode
;; hooks to fill-column values.
(dolist (hook '(org-mode-hook
                text-mode-hook))
  (add-hook hook #'(lambda ()
                     (auto-fill-mode 1)
                     (setq fill-column 78))))

(provide 'setup-auto-fill)
