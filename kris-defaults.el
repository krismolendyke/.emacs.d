;; Show the active region.
(transient-mark-mode t)

;; Where am I?
(line-number-mode t)
(column-number-mode t)
(show-paren-mode t)

(ido-mode t)

;; Tabs... *hisssss*
(set-default 'indent-tabs-mode nil)

;; Kill trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Quit all that damn racket!
(setq ring-bell-function 'ignore)

;; Make sure syntax highlighting is enabled.
(global-font-lock-mode t)

;; y is the new yes.  n is the new no.
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'kris-defaults)
