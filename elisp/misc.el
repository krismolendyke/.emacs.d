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

;; Inspired by https://github.com/purcell/exec-path-from-shell
(defun set-path-from-shell ()
  "Set Emacs' PATH and exec-path variables to the current PATH
value in the shell."
  (interactive)
  (setq exec-path
        (split-string
         (cadr (reverse
                (split-string
                 (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'")
                 "\n")))
         ":")))
