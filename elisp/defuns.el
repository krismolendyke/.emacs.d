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

(defun file-contents-as-string (filename)
  "Return the given FILENAME contents as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun dirs-in-dir (directory &optional excludes)
  "Non-recursively list directories in DIRECTORY.

Optionally exclude EXCLUDES from the result list."
  (let ((files (directory-files directory t nil t))
        (excludes (or excludes '()))
        (dirs '()))
    (dolist (f files dirs)
      (when (and (file-directory-p f)
                 (not (member (file-name-nondirectory f) excludes)))
        (push f dirs)))))

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

(defun blank-line-p ()
  "Is point currently on a blank line?"
  (looking-at "[ \t]*$"))

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
(require 'tramp)

(defun known-hosts ()
  "Get a host name from ~./ssh/known_hosts file."
  (completing-read "host: "
                   (let ((value))
                     (dolist (elt (tramp-parse-shosts "~/.ssh/known_hosts") value)
                       (if elt (setq value (cons (cadr elt) value)))))))

(defun host-ip ()
  "Insert the current IP of a host using `dns-lookup-program'.
Similar to but simpler than `dns-lookup-host'."
  (interactive)
  (let ((host (known-hosts)))
    (insert (car (last (split-string (shell-command-to-string
                                      (concat dns-lookup-program " " host))))))))

(provide 'misc)

;;; misc.el ends here