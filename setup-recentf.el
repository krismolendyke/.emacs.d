(require 'recentf)

(setq recentf-max-saved-items 250)
(recentf-mode 1)

;;; Adapted from http://emacsredux.com/blog/2013/04/05/recently-visited-files
(defun recentf-ido-find-file ()
  "Find a recently opened file with ido."
  (interactive)
  (let ((file (ido-completing-read "Find recent file: " recentf-list nil t)))
    (if file (find-file file))))

(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(provide 'setup-recentf)
