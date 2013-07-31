(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'paredit)

;; Awesome advice to safe buffers before loading or running tests
;; courtesy of
;; https://github.com/magnars/.emacs.d/blob/486e631801c84b018d90cf040d2170ef78045676/setup-clojure-mode.el
(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(add-hook 'clojure-mode-hook 'paredit-mode)

(provide 'setup-clojure)
