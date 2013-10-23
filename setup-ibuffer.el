(require 'ibuf-ext)

(setq ibuffer-formats '((mark
                         " "
                         (modified)
                         " "
                         (name 40 40 :right :elide)
                         " "
                         (filename-and-process))
                        (mark
                         " "
                         (filename-and-process 70 70 :left :elide)
                         " "
                         name)))

(add-hook 'ibuffer-hook (lambda () (ibuffer-switch-to-saved-filter-groups "")))

(provide 'setup-ibuffer)
