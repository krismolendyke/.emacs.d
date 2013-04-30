(require 'flycheck)

;; Easier navigation for errors/warnings/etc.
;; ◀◀
(global-set-key (kbd "<f7>") 'flycheck-previous-error)
;; ▶▶
(global-set-key (kbd "<f9>") 'flycheck-next-error)

(provide 'setup-flycheck)
