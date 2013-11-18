(require 'flycheck)
(require 'monetate-mode)
(require 'yasnippet)

(add-hook 'js-mode-hook (lambda ()
                          (linum-mode 1)
                          (flycheck-mode 1)
                          (yas-minor-mode 1)))

(defun js-nearest-function-name ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((defun-line (car (split-string (buffer-substring (point) (point-at-eol)) "="))))
      (if (string-match "[ \t\n\r]+\\'" defun-line)
          (replace-match "" t t defun-line)
        defun-line))))

(defun js-log-group-function ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-line 1)
    (if (monetate-server-repo-p) (insert "mt."))
    (insert (format "console.group('%s');" (js-nearest-function-name)))
    (newline)
    (unless (blank-line-p) (newline-and-indent))
    (beginning-of-defun)
    (end-of-defun)
    (forward-line -1)
    (unless (blank-line-p)
      (end-of-line)
      (newline-and-indent))
    (newline-and-indent)
    (if (monetate-server-repo-p) (insert "mt."))
    (insert "console.groupEnd();")))

(provide 'setup-js)
