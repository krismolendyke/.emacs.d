(require 'calendar)
(require 'lunar)

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

(defun full-moons-info ()
  "Get a list of upcoming full moons information beginning with
the current month.

See `lunar-phase-list' and `lunar-phase-name'."
  (let* ((current-date (calendar-current-date))
         (current-month (car current-date))
         (current-year (car (last current-date)))
         (full-moon-phase-index 2)
         (full-moons-info '()))
    (dolist (phase (lunar-phase-list current-month current-year))
      (if (= (car (last phase)) full-moon-phase-index)
          (setq full-moons-info (cons phase full-moons-info))))
    (reverse full-moons-info)))

(defun full-moons ()
  "Display a buffer containing upcoming full moons beginning with
the current month."
  (interactive)
  (with-output-to-temp-buffer "*full-moons*"
    (princ
     (mapconcat
      #'(lambda (x)
          (format "%s %s" (calendar-date-string (car x)) (car (cdr x))))
      (full-moons-info)
      "\n"))))

(provide 'misc)
