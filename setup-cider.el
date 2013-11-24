(require 'cider)
(require 'paredit)

(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (paredit-mode +1)))
(setq nrepl-hide-special-buffers t
      cider-repl-popup-stacktraces nil
      cider-repl-use-pretty-printing t
      cider-repl-history-file (expand-file-name "nrepl-history" dropbox-directory))

(provide 'setup-cider)
