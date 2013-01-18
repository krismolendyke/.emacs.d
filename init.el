;; Turn off bars A.S.A.P.  See appearance.el for more.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Only turn off the menu bar in the terminal.
(if (and
     (fboundp 'window-system)
     (not (window-system))
     (fboundp 'menu-bar-mode))
    (menu-bar-mode -1))

;; Set paths to dependencies.
;; Stuff that other folks have developed.
(setq site-lisp-directory (expand-file-name "site-lisp" user-emacs-directory))
;; Stuff that I have developed.
(setq elisp-directory (expand-file-name "elisp" user-emacs-directory))

;; Set up load path.
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-directory)
(add-to-list 'load-path elisp-directory)

;; Add external projects to load path.
(dolist (project (directory-files site-lisp-directory t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load my stuff.
(dolist (file (directory-files elisp-directory t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Fixup any path problems Emacs.app may have.
(set-path-from-shell)

;; Emacs custom settings are in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Cloud storage.
(setq dropbox-directory (expand-file-name "~/Desktop/Dropbox"))
(setq google-drive-directory (expand-file-name "~/Google Drive"))

;; ibuffer.
(defalias 'list-buffers 'ibuffer)

;; Theme, font, frame attributes, etc.
(require 'appearance)

;; Kris's defaults.
(require 'kris-defaults)

;; I spend most of my time in OS X.
(if (equal system-type 'darwin) (require 'osx))

;; Start the Emacs server.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Setup extensions.
(require 'setup-ace-jump)
(require 'setup-auto-fill)
(require 'setup-calendar)
(require 'setup-clojure)
(require 'setup-dired)
(require 'setup-electric-indent)
(require 'setup-flyspell)
(require 'setup-git)
(require 'setup-hyperspec)
(require 'setup-ibuffer)
(require 'setup-ido)
(require 'setup-js2)
(require 'setup-markdown)
(require 'setup-nrepl)
(require 'setup-org)
(require 'setup-paredit)
(require 'setup-powerline)
(require 'setup-python)
(require 'setup-slime)
(require 'setup-smex)
(require 'setup-windmove)
