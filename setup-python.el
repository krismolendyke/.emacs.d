(require 'autopair)
(require 'flycheck)
(require 'python)

(add-hook 'python-mode-hook
          #'(lambda ()
              ;; Do not drive me crazy with extra-dumb indentation!
              (setq electric-indent-inhibit t)
              (linum-mode 1)
              ;; (flycheck-mode 1)
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

;; Use IPython!
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(provide 'setup-python)
