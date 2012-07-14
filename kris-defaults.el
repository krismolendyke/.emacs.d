;; Show the active region.
(transient-mark-mode t)

;; Where am I?
(line-number-mode t)
(column-number-mode t)
(show-paren-mode t)

;; Tabs... *hisssss*
(set-default 'indent-tabs-mode nil)

;; Kill trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Dvorak C-x help.  Plus, I hate C-z minimizing the window on OS X.
(global-set-key (kbd "C-z") ctl-x-map)

;; Give a man a hand.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Quit all that damn racket!
(setq ring-bell-function 'ignore)

;; Make sure syntax highlighting is enabled.
(global-font-lock-mode t)

;; y is the new yes.  n is the new no.
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'kris-defaults)
