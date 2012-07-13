(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/Code/slime/")
(add-to-list 'load-path "~/Code/slime/contrib/")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(provide 'setup-slime)
