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

(defvar site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "Local libraries.")

(defvar elisp-directory
  (expand-file-name "elisp" user-emacs-directory)
  "Stuff that I have developed.")

(defvar dropbox-directory
  (expand-file-name "~/Desktop/Dropbox")
  "Dropbox home.")

(defvar google-drive-directory
  (expand-file-name "~/Google Drive")
  "Google Drive home.")

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


;; Remember and restore buffer/file/etc. state between sessions.
(require 'desktop)
(setq desktop-path (list dropbox-directory)
      desktop-load-locked-desktop t)
(desktop-save-mode 1)
(desktop-read dropbox-directory)



;; org-mode Emacs Lisp files.
(setq org-lisp-directory
      (expand-file-name "lisp"
                        (expand-file-name "org-mode"
                                          site-lisp-directory)))
(add-to-list 'load-path org-lisp-directory)

(defun init-from-org ()
  (require 'org)
  (dolist (elt (directory-files user-emacs-directory t "\\.org$" t))
    (org-babel-load-file elt t)))

(add-hook 'after-init-hook 'init-from-org)
