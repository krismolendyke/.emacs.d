(require 'exec-path-from-shell)

;; Fix environment issues with Emacs.app.
(add-to-list 'exec-path-from-shell-variables "DEVBOX")
(exec-path-from-shell-initialize)

;; Command as meta.
(setq ns-command-modifier 'meta)

;; Option as hyper.
(setq ns-option-modifier 'hyper)

;; fn as super.
(setq ns-function-modifier 'super)

 ;; Trackpad taming.
(setq
 mouse-wheel-scroll-amount '(0.0001)
 mouse-wheel-progressive-speed nil
 scroll-step 1
 scroll-conservatively 10000
 auto-window-vscroll nil)

(provide 'osx)
