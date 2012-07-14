;; Turn off bars A.S.A.P.  See appearance.el for more.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Only turn off the menu bar in the terminal.
(if (and (not (window-system)) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; Set path to .emacs.d.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies.
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Set up load path.
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path.
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Emacs custom settings are in a separate file.
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Theme, font, frame attributes, etc.
(require 'appearance)

;; Kris's defaults.
(require 'kris-defaults)

;; Setup extensions.
(require 'setup-paredit)
(require 'setup-slime)

(require 'smex)
(smex-initialize)
