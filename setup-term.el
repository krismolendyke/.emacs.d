(require 'term)

;; `autopair-mode' interferes with `term-send-raw' bounding on `RET'.
(if (fboundp 'autopair-mode)
    (add-hook 'term-mode-hook (lambda () (autopair-mode -1))))

(defun zsh ()
  "Run an `ansi-term' process with zsh."
  (interactive)
  (ansi-term "/bin/zsh" "zsh"))

(provide 'setup-term)
