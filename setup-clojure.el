(require 'clojure-mode)
(require 'paredit)

;; ClojureScript!
(setq auto-mode-alist
      (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

(add-hook 'clojure-mode-hook 'paredit-mode)

(provide 'setup-clojure)
