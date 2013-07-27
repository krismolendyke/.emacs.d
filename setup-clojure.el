(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'paredit)

(add-hook 'clojure-mode-hook 'paredit-mode)

(provide 'setup-clojure)
