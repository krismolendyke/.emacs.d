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

(provide 'setup-ibuffer)
