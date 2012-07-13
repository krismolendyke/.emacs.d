;; Show the active region.
(transient-mark-mode t)

;; Where am I?
(line-number-mode t)
(column-number-mode t)
(show-paren-mode t)

(ido-mode t)

;; Quit all that damn racket!
(setq ring-bell-function 'ignore)

;; Set HyperSpec root in Dropbox.
(setq common-lisp-hyperspec-root
      "file:///Users/kris/Desktop/Dropbox/Documents/HyperSpec/")
(load "newcomment") 

;; Make sure syntax highlight is enabled.
(global-font-lock-mode t)

;; y is the new yes.  n is the new no.
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'kris-defaults)
