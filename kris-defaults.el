;; Please, no backup or autosave litter.
(setq make-backup-files nil
      auto-save-default nil)

;; Show the active region.
(transient-mark-mode t)

;; Overwrite text when typing in a region.
(delete-selection-mode 1)

;; Prefer splitting windows horizontally.
(setq split-width-threshold 81)

;; What's going on here?
(setq echo-keystrokes 0.1)

;; quoted-insert is a frequent typo of mine.
(global-unset-key (kbd "C-q"))

;; Text mode menu bar drives me nuts!
(global-unset-key (kbd "M-`"))

;; I find that I join lines often.
(global-set-key (kbd "C-c DEL") 'join-line)

;; Easier git-blame-mode access.
(global-set-key (kbd "C-x v b") 'git-blame-mode)

;; find-function instead of Emacs FAQ.
(global-set-key (kbd "C-h C-f") 'find-function)

;; find-variable instead of ?
(global-set-key (kbd "C-h C-v") 'find-variable)

;; Toggle fullscreen mode on OS X
(if (fboundp 'ns-toggle-fullscreen)
    (global-set-key (kbd "C-x C-t") 'ns-toggle-fullscreen))

;; Where am I?
;(global-linum-mode t)
(line-number-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(show-paren-mode t)

;; Please no more <2>, <3>, >_<
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Do not break lines.  Truncate them.
(setq-default truncate-lines t)

;; Automatically reload buffers when files change on disk.
(global-auto-revert-mode 1)

;; Tabs... *hisssss*
(set-default 'indent-tabs-mode nil)

;; Nag, nag, nag, nag...
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; Kill trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(setq-default show-trailing-whitespace nil)

;; Require a final newline before saving or writing a file.
(setq require-final-newline t)
(setq mode-require-final-newline t)

;; Give a man a hand.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Quit all that damn racket!
(setq ring-bell-function 'ignore)

;; Make sure syntax highlighting is enabled.
(global-font-lock-mode t)

;; y is the new yes.  n is the new no.
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'kris-defaults)
