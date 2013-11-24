(require 'exec-path-from-shell)

;; Fix environment issues with Emacs.app.
(add-to-list 'exec-path-from-shell-variables "DEVBOX")
(exec-path-from-shell-initialize)

(provide 'setup-path)
