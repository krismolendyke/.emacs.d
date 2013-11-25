(require 'smex)
(require 'term)

;; `autopair-mode' interferes with `term-send-raw' bounding on `RET'.
(if (fboundp 'autopair-mode)
    (add-hook 'term-mode-hook (lambda () (autopair-mode -1))))

;; I have yet to need execute-extended-command in the terminal.
(define-key term-raw-map [?\M-x] 'smex)
(define-key term-mode-map [?\M-x] 'smex)

(defun zsh ()
  "Run an `ansi-term' process with zsh."
  (interactive)
  (ansi-term "/bin/zsh" "zsh"))

(provide 'setup-term)
