;;; defuns.el --- miscellaneous functions

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
(defun blank-line-p ()
  "Is point currently on a blank line?"
  (looking-at "[ \t]*$"))

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

;;; defuns.el ends here
