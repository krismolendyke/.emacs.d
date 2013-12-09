(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

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

;; Setup PATH
(require 'setup-path)

;; Emacs custom settings are in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Cloud storage.
(setq dropbox-directory (expand-file-name "~/Desktop/Dropbox"))
(setq google-drive-directory (expand-file-name "~/Google Drive"))

;; Save minibuffer history.
(setq savehist-file (expand-file-name ".savehist" dropbox-directory))
(savehist-mode)

;; Homebrew source files.
(setq source-directory "/Library/Caches/Homebrew/emacs--git")



;; org-mode Emacs Lisp files.
(setq org-lisp-directory
      (expand-file-name "lisp"
                        (expand-file-name "org-mode"
                                          site-lisp-directory)))
(add-to-list 'load-path org-lisp-directory)
(require 'org)
(mapc #'org-babel-load-file
      (directory-files user-emacs-directory t "\\.org$" t))



;; I spend most of my time in OS X.
(if (equal system-type 'darwin) (require 'osx))

;; Remember window configurations.
(winner-mode 1)

;; ibuffer.
(defalias 'list-buffers 'ibuffer)

;; Theme, font, frame attributes, etc.
(require 'appearance)

;; Kris's defaults.
(require 'kris-defaults)

;; Remember and restore buffer/file/etc. state between sessions.
(setq desktop-path (list dropbox-directory)
      desktop-load-locked-desktop t)
(desktop-save-mode 1)
(desktop-read dropbox-directory)

;; Start the Emacs server.
(require 'server)
(unless (server-running-p)
  (server-start))
