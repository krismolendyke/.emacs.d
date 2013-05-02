(require 'flycheck)

(add-hook 'js-mode-hook (lambda ()
                          (linum-mode 1)
                          (flycheck-mode 1)))

(provide 'setup-js)
