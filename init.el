;;; init.el --- Bootstrap initialization.

;;; Commentary:
;;
;; Bootstrap setup during initialization time.  Advanced configuration
;; and setup is done in an `after-init-hook' function.

;;; Code:

(require 'subr-x)

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

(defvar k20e/google-drive-directory
  (expand-file-name "~/Google")
  "Google Drive home.")

(when (string-equal system-type "darwin")
                    ; TODO move into macos section in custom
  (setq mac-command-modifier 'meta
        ns-alternate-modifier 'super
        shell-file-name "/opt/homebrew/bin/bash")
  (defvar k20e/brew-cache-directory
    (string-trim (shell-command-to-string
                  (string-join `(,(executable-find "brew") "--cache") " ")))
    "Homebrew cache."))

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
  (dolist (directory (list k20e/site-lisp-directory
                           k20e/elisp-directory
                           k20e/org-lisp-directory
                           k20e/org-lisp-contrib-directory))
    (add-to-list 'load-path directory))

  ;; Add external projects to load path.
  (dolist (project (directory-files k20e/site-lisp-directory t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

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
  (require 'org)
  (dolist (elt (directory-files user-emacs-directory t "\\.org$" t))
    (org-babel-load-file elt t)))

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

;;; Set the Emacs source directory so that C function source can be
;;; found when necessary.
(when (boundp 'k20e/brew-cache-directory)
  (setq source-directory
        (string-join `(,k20e/brew-cache-directory "emacs--git") "/")))

(k20e/no-bars-held)
(k20e/setup-load-path)
(k20e/setup-use-package)
(k20e/setup-exec-path)
(k20e/load-custom-elisp)
(k20e/load-custom-org)
(k20e/restore-desktop)
(add-hook 'after-init-hook 'k20e/after-init-hook)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
