;; Turn off bars A.S.A.P.
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Theming
(load-theme 'wombat t)
(set-frame-font "-apple-Consolas-medium-normal-normal-*-18-*-*-*-m-0-fontset-auto1")

;; Maximize the height of the window based on the current screen resolution.
(defun get-max-rows (pixel-height)
  "Return the maximum number of rows that will fit with this screen.
Given a screen pixel height at the current frame character height, calculate
the maximum number of rows that will fit with that height."
  (if (window-system)
      (/ pixel-height (frame-char-height))))

(defun set-frame-height-to-max ()
  "Set the selected frame height to the maximum that will fit the current
screen resolution."
  (if (window-system)
      (set-frame-height (selected-frame)
			(get-max-rows (- (display-pixel-height) 44)))))

(set-frame-height-to-max)

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies.
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Set up load path.
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Emacs custom settings are in a separate file.
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Kris
(transient-mark-mode t)
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)
(setq ring-bell-function 'ignore)
(setq common-lisp-hyperspec-root
      "file:///Users/kris/Desktop/Dropbox/Documents/HyperSpec/")
(load "newcomment")

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
