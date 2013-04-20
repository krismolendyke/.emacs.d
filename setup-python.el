(require 'autopair)
(require 'python)

(add-hook 'python-mode-hook
          #'(lambda ()
              (linum-mode 1)
              (setq fill-column 118
                    autopair-handle-action-fns (list #'autopair-default-handle-action
                                                     #'autopair-python-triple-quote-action))
              ;; Previously:
              ;; C-M-f, C-M-b (paredit-forward/back)
              ;; C-M-n, C-M-p (forward-list/backward-list)
              ;; C-M-a, C-M-e (beginning-of-defun/end-of-defun)
              (define-key python-mode-map (kbd "M-a") 'python-nav-beginning-of-statement)
              (define-key python-mode-map (kbd "M-e") 'python-nav-end-of-statement)
              (define-key python-mode-map (kbd "M-n") 'python-nav-forward-statement)
              (define-key python-mode-map (kbd "M-p") 'python-nav-backward-statement)
              (define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-sexp)
              (define-key python-mode-map (kbd "C-M-b") '(lambda () (interactive) (python-nav--backward-sexp)))
              (define-key python-mode-map (kbd "C-M-n") 'python-nav-forward-block)
              (define-key python-mode-map (kbd "C-M-p") 'python-nav-backward-block)))

(provide 'setup-python)
