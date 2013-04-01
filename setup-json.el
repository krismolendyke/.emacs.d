(add-to-list 'auto-mode-alist '("\\.json" . js-mode))

(defun json-format ()
  "Pretty-print a buffer containing JSON."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "python -m json.tool" (current-buffer)))

(provide 'setup-json)
