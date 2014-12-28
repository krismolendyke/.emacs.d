;;; defuns.el --- miscellaneous functions

;;; Commentary:
;;
;; Most of these functions are on a sort of "trial run" until I decide if I
;; really need them around in a more permanent module.  There are a few key
;; bindings to go along with a few functions as well.

;;; Code:
(require 'files)

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

;;; defuns.el ends here
