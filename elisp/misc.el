;;; misc.el --- miscellaneous functions

;;; Commentary:
;;
;; Most of these functions are on a sort of "trial run" until I decide if I
;; really need them around in a more permanent module.  There are a few key
;; bindings to go along with a few functions as well.

;;; Code:
(require 'calendar)
(require 'files)
(require 'lunar)

;; A few nice editing functions.
(defun open-line-below ()
  "Insert a new line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Insert a new line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<M-return>") 'open-line-below)
(global-set-key (kbd "<M-S-return>") 'open-line-above)

;; Inspired by http://whattheemacsd.com/key-bindings.el-01.html
(defun goto-linum ()
  "Show line numbers and prompt for a line number to go to."
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
  "Set `exec-path' to the current PATH value in the shell."
  (interactive)
  (setq exec-path
        (parse-colon-path (cadr (reverse (split-string
                                          (shell-command-to-string
                                           "$SHELL --login -i -c 'echo $PATH'")
                                          "\n"))))))

(defun full-moons-info ()
  "Get a list of upcoming full moons info beginning with the current month.
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
  "Display upcoming full moons beginning with the current month."
  (interactive)
  (with-output-to-temp-buffer "*full-moons*"
    (princ
     (mapconcat
      #'(lambda (x)
          (format "%s %s" (calendar-date-string (car x)) (car (cdr x))))
      (full-moons-info)
      "\n"))))

(require 'net-utils)

(defun host-ip (host)
  "Insert the current IP of the given HOST using `dns-lookup-program'.
Similar to but simpler than `dns-lookup-host'."
  (interactive "*sHost: ")
  (insert (car (last (split-string (shell-command-to-string
                                    (concat dns-lookup-program " " host)))))))

(provide 'misc)

;;; misc.el ends here
