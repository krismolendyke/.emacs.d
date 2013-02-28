(require 'multiple-cursors)

;; Keep preferences sync'd across machines.
(setq mc/list-file (expand-file-name ".mc-lists.el" dropbox-directory))

(global-set-key (kbd "M-L") 'mc/edit-lines)
(global-set-key (kbd "C-M-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

(defun mark-current-line ()
  "Mark the current line.
If the mark is already set simply move the point forward a single
line.  If it is not set, set it at the beginning of the current
line and then move the point forward a single line."
  (interactive)
  (unless mark-active
    (beginning-of-line)
    (set-mark (point)))
  (forward-line 1))

(global-set-key (kbd "M-l") 'mark-current-line)

(provide 'setup-multiple-cursors)
