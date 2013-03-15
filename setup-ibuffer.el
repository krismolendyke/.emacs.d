(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (setq ibuffer-truncate-lines t)))

(setq ibuffer-formats '((mark modified read-only " "
                              (name 24 24 :right :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))

(provide 'setup-ibuffer)
