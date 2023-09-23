;;; init.el --- Bootstrap initialization.

;;; Commentary:
;;
;; Bootstrap setup during initialization time.  Advanced configuration
;; and setup is done in an `after-init-hook' function.

;;; Code:

(require 'subr-x)

(defvar k20e/elisp-directory
  (expand-file-name "elisp" user-emacs-directory)
  "Stuff that I have developed.")

(defvar k20e/google-drive-directory
  (expand-file-name "~/Google")
  "Google Drive home.")

(defun k20e/no-bars-held ()
  "Turn off tool, scroll, and menu bars when appropriate.
Only turn off the menu bar running in a terminal window."
  (setq inhibit-startup-echo-area-message t
        inhibit-startup-screen t)
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (and (fboundp 'window-system)
           (not (window-system))
           (fboundp 'menu-bar-mode))
      (menu-bar-mode 0)))

(defun k20e/setup-load-path ()
  "Add custom directories to `load-path'."
  (dolist (directory (list k20e/elisp-directory))
    (add-to-list 'load-path directory)))

(defun k20e/setup-use-package ()
  "Configure use-package."
  (use-package package
    :config
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)))

(defun k20e/setup-exec-path ()
  "Setup `exec-path'."
  (use-package exec-path-from-shell
    :ensure t)
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                '("DYLD_LIBRARY_PATH" ; https://github.com/rust-lang-nursery/rustfmt#tips
                  "GOPATH"
                  "GOROOT"
                  "SHELL")))
  (exec-path-from-shell-initialize))

(defun k20e/load-custom-elisp ()
  "Load custom Emacs Lisp files in `k20e/elisp-directory'."
  (dolist (file (directory-files k20e/elisp-directory t "\\w+"))
    (when (file-regular-p file)
      (load file))))

(defun k20e/load-custom-org ()
  "Load custom Org Mode configuration."
  (use-package org
    :ensure t
    :pin manual
    :load-path "site-lisp/org-mode/lisp"
    :config
    (dolist (elt (directory-files user-emacs-directory t "\\.org$" t))
    (org-babel-load-file elt t))))

(defun k20e/restore-desktop ()
  "Restore the state of buffers from the last session."
  (require 'desktop)
  (setq-default dired-use-ls-dired nil)
  (setq desktop-path (list k20e/google-drive-directory)
        desktop-load-locked-desktop t)
  (desktop-save-mode 1)
  (desktop-read k20e/google-drive-directory))

(defun k20e/after-init-hook ()
  "Perform complex post-initialization.")

;;; MacOS specific configuration.
(use-package emacs
  :when (eq system-type 'darwin)
  :config
  (setq-default mac-command-modifier 'meta
                mac-option-modifier 'super
                mac-control-modifier 'control
                mac-function-modifier 'hyper
                shell-file-name "/opt/homebrew/bin/bash"))

;;; Other settings
(use-package emacs
  :config
  (setq-default custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory)))

(k20e/no-bars-held)
(k20e/setup-load-path)
(k20e/setup-use-package)
(k20e/setup-exec-path)
(k20e/load-custom-elisp)
(k20e/load-custom-org)
(k20e/restore-desktop)
(add-hook 'after-init-hook 'k20e/after-init-hook)

;;; init.el ends here
