;; Inspired by http://whattheemacsd.com/key-bindings.el-01.html
(defun goto-linum ()
  "Show line numbers before prompting for a line to go to."
  (interactive)
  (let ((linum-mode-previous-state
         (if (and (boundp 'linum-mode) linum-mode) 1 -1)))
    (unwind-protect
        (progn
          (linum-mode 1)
          (call-interactively 'goto-line)
          (linum-mode linum-mode-previous-state))
      (linum-mode linum-mode-previous-state))))

(global-set-key [remap goto-line] 'goto-linum)
