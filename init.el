;;; init.el --- Bootstrap initialization.

;;; Commentary:
;;
;; Bootstrap setup during initialization time.  Advanced configuration
;; and setup is done in an `after-init-hook' function.

;;; Code:

(defvar site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "Local libraries.")

(defvar elisp-directory
  (expand-file-name "elisp" user-emacs-directory)
  "Stuff that I have developed.")

(defvar org-lisp-directory
  (expand-file-name "lisp" (expand-file-name "org-mode" site-lisp-directory))
  "The directory contaiting `org-mode' Emacs Lisp files.")

(defvar dropbox-directory
  (expand-file-name "~/Desktop/Dropbox")
  "Dropbox home.")

(defvar google-drive-directory
  (expand-file-name "~/Google Drive")
  "Google Drive home.")

(defun setup-cask-and-pallet ()
  "Package managment goodness."
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (require 'pallet))

(defun no-bars-held ()
  "Turn off tool, scroll, and menu bars when appropriate.
Only turn off the menu bar running in a terminal window."
  (setq inhibit-startup-echo-area-message "kris")
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (and (fboundp 'window-system)
           (not (window-system))
           (fboundp 'menu-bar-mode))
      (menu-bar-mode -1)))

(defun setup-load-path ()
  "Add custom directories to `load-path'."
  (dolist (directory (list site-lisp-directory
                           elisp-directory
                           org-lisp-directory))
    (add-to-list 'load-path directory))

  ;; Add external projects to load path.
  (dolist (project (directory-files site-lisp-directory t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

(defun load-custom-elisp ()
  "Load custom Emacs Lisp files in `elisp-directory'."
  (dolist (file (directory-files elisp-directory t "\\w+"))
    (when (file-regular-p file)
      (load file))))

(defun restore-desktop ()
  "Restore the state of buffers from the last session."
  (require 'desktop)
  (setq desktop-path (list dropbox-directory)
        desktop-load-locked-desktop t)
  (desktop-save-mode 1)
  (desktop-read dropbox-directory))

(defun after-init ()
  "Perform complex post-initialization."
  (require 'org)
  (dolist (elt (directory-files user-emacs-directory t "\\.org$" t))
    (org-babel-load-file elt t)))

(setq source-directory "/Library/Caches/Homebrew/emacs--git")
(setup-cask-and-pallet)
(no-bars-held)
(setup-load-path)
(load-custom-elisp)
(restore-desktop)
(add-hook 'after-init-hook 'after-init)

;;; init.el ends here
