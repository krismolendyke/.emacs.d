;; Please, no backup or autosave litter.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Show the active region.
(transient-mark-mode t)

;; What's going on here?
(setq echo-keystrokes 0.1)

;; Where am I?
;(global-linum-mode t)
(line-number-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(show-paren-mode t)

;; Do not wrap lines in the middle of tokens, please.
(global-visual-line-mode 1)

;; Automatically reload buffers when files change on disk.
(global-auto-revert-mode 1)

;; Tabs... *hisssss*
(set-default 'indent-tabs-mode nil)

;; Nag, nag, nag, nag...
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Kill trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Give a man a hand.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Quit all that damn racket!
(setq ring-bell-function 'ignore)

;; Make sure syntax highlighting is enabled.
(global-font-lock-mode t)

;; y is the new yes.  n is the new no.
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'kris-defaults)
