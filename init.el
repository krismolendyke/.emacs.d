;;; init.el --- Bootstrap initialization.

;;; Commentary:
;;
;; Bootstrap setup during initialization time.  Advanced configuration
;; and setup is done in an `after-init-hook' function.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar k20e/site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "Local libraries.")

(defvar k20e/elisp-directory
  (expand-file-name "elisp" user-emacs-directory)
  "Stuff that I have developed.")

(defvar k20e/org-mode-directory
  (expand-file-name "org-mode" k20e/site-lisp-directory)
  "The directory containing `org-mode' files.
This is a custom location to keep `org-mode' as a git submodule
decoupled from the Emacs distribution package.")

(defvar k20e/org-lisp-directory
  (expand-file-name "lisp" k20e/org-mode-directory)
  "The directory contaiting `org-mode' Emacs Lisp files.")

(defvar k20e/org-lisp-contrib-directory
  (expand-file-name "lisp" (expand-file-name "contrib" k20e/org-mode-directory))
  "The directory containing `org-mode' Emacs Lisp add-on files.")

(defvar k20e/dropbox-directory
  (expand-file-name "~/Desktop/Dropbox")
  "Dropbox home.")

(defvar k20e/google-drive-directory
  (expand-file-name "~/Google Drive")
  "Google Drive home.")

(defvar k20e/cask-directory
  (expand-file-name "~/.cask")
  "Cask home.")

(defun k20e/setup-cask-and-pallet ()
  "Package management goodness."
  (require 'cask (expand-file-name "cask.el" k20e/cask-directory))
  (cask-initialize)
  (require 'pallet)
  (pallet-mode t))

(defun k20e/no-bars-held ()
  "Turn off tool, scroll, and menu bars when appropriate.
Only turn off the menu bar running in a terminal window."
  (setq inhibit-startup-echo-area-message "kris")
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (and (fboundp 'window-system)
           (not (window-system))
           (fboundp 'menu-bar-mode))
      (menu-bar-mode -1)))

(defun k20e/setup-load-path ()
  "Add custom directories to `load-path'."
  (dolist (directory (list k20e/site-lisp-directory
                           k20e/elisp-directory
                           k20e/org-lisp-directory
                           k20e/org-lisp-contrib-directory))
    (add-to-list 'load-path directory))

  ;; Add external projects to load path.
  (dolist (project (directory-files k20e/site-lisp-directory t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

(defun k20e/setup-exec-path ()
  "Setup `exec-path'."
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                '("DEVBOX"
                  "GOPATH"
                  "GOROOT")))
  (exec-path-from-shell-initialize))

(defun k20e/load-custom-elisp ()
  "Load custom Emacs Lisp files in `k20e/elisp-directory'."
  (dolist (file (directory-files k20e/elisp-directory t "\\w+"))
    (when (file-regular-p file)
      (load file))))

(defun k20e/restore-desktop ()
  "Restore the state of buffers from the last session."
  (require 'desktop)
  (setq-default dired-use-ls-dired nil)
  (setq desktop-path (list k20e/dropbox-directory)
        desktop-load-locked-desktop t)
  (desktop-save-mode 1)
  (desktop-read k20e/dropbox-directory))

(defun k20e/after-init-hook ()
  "Perform complex post-initialization."
  (require 'org)
  (dolist (elt (directory-files user-emacs-directory t "\\.org$" t))
    (org-babel-load-file elt t)))

;;; Set the Emacs source directory so that C function source can be
;;; found when necessary.
(setq source-directory "/Library/Caches/Homebrew/emacs--git")

(k20e/setup-cask-and-pallet)
(k20e/no-bars-held)
(k20e/setup-load-path)
(k20e/setup-exec-path)
(k20e/load-custom-elisp)
(k20e/restore-desktop)
(add-hook 'after-init-hook 'k20e/after-init-hook)

;;; init.el ends here
