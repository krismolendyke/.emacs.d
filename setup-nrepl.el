(require 'clojure-mode)
(require 'nrepl)
(require 'paredit)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(setq nrepl-hide-special-buffers t
      nrepl-popup-stacktraces-in-repl nil
      nrepl-use-pretty-printing t
      nrepl-history-file (expand-file-name "nrepl-history" dropbox-directory))
(nrepl-enable-on-existing-clojure-buffers)

(provide 'setup-nrepl)
