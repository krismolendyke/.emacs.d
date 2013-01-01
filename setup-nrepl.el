(require 'clojure-mode)
(require 'nrepl)
(require 'paredit)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

(provide 'setup-nrepl)
