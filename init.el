;; Load path
(add-to-list 'load-path "~/Library/Emacs/site-lisp")

;; Kris
(transient-mark-mode t)
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)
(setq common-lisp-hyperspec-root
      "file:///Users/kris/Desktop/Dropbox/Documents/HyperSpec/")
(load "newcomment")

;; Stylin'
(load-theme 'wombat t)
(set-frame-font "-apple-Consolas-medium-normal-normal-*-18-*-*-*-m-0-fontset-auto1")

(defun get-max-rows (pixel-height)
  "Return the maximum number of rows that will fit in the given screen pixel heightat the current frame character height."
  (if (window-system)
      (/ pixel-height (frame-char-height))))

(defun set-frame-height-to-max ()
  "Set the selected frame height to the maximum that will fit the current screen resolution."
  (if (window-system)
      (set-frame-height (selected-frame)
			(get-max-rows (- (display-pixel-height) 100)))))

(set-frame-height-to-max)

;; paredit
(autoload 'paredit-mode "paredit" nil t)

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook hook #'(lambda nil (paredit-mode 1))))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [?\)] 'paredit-close-parenthesis)
     (define-key paredit-mode-map [(meta ?\))]
                 'paredit-close-parenthesis-and-newline)))

;; SLIME!
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/Code/slime/")
(add-to-list 'load-path "~/Code/slime/contrib/")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-scratch-message))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
