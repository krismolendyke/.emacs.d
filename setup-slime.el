(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/Code/slime/")
(add-to-list 'load-path "~/Code/slime/contrib/")

(require 'slime-autoloads)

;; The slime-fancy meta-package loads and initializes the following packages:
;;
;; slime-repl
;; slime-autodoc
;; slime-c-p-c
;; slime-editing-commands
;; slime-fancy-inspector
;; slime-fuzzy
;; slime-presentations
;; slime-scratch
;; slime-references
;; slime-package-fu
;; slime-fontifying-fu
;;
;; The is currently a conflict between slime-c-p-c and slime-js which
;; corrupts the slime-js completion feature.
;;
;; For now I'm going to trade slime-c-p-c completion for slime-js completion
;; by loading everything in slime-fancy except for slime-c-p-c.

;; Would like this to work ;)
;; (slime-setup '(slime-fancy slime-js))

;; Mimic slime-fancy without slime-c-p-c.
(slime-setup
 '(slime-repl
   slime-autodoc
                                        ; slime-c-p-c
   slime-editing-commands
   slime-fancy-inspector
   slime-fuzzy
   slime-presentations
   slime-scratch
   slime-references
   slime-package-fu
   slime-fontifying-fu
   slime-js))

(add-hook 'sldb-hook #'(lambda () (autopair-mode -1)))

(provide 'setup-slime)
