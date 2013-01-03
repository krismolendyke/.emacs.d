;;; widescreen.el --- adjust windows on widescreen monitors

;;; Commentary:
;;
;; When working on a widescreen monitor it can be useful to have
;; windows arranged a bit differently than they would on smaller
;; monitors.  In particular, a function like `fit-window-to-buffer'
;; which adjusts the window's width is helpful.

;;; Code:

(defun get-longest-line-length ()
  "Get the length of the longest line in the selected window."
  (save-excursion
    (beginning-of-buffer)
    (let ((max-length 0)
          (last-line (count-lines (point-min) (point-max))))
      (while (<= (line-number-at-pos) last-line)
        (setq max-length (max max-length (- (point-at-eol) (point-at-bol))))
        (forward-line))
      (1+ max-length))))

(defun fit-window-to-buffer-horizontally ()
  "Fit the selected window to the width of its longest line.
Return the window width delta."
  (interactive)
  (let* ((current-width (window-width))
         (longest-line (get-longest-line-length))
         (delta (* -1 (- current-width longest-line))))
    (if (zerop (window-resizable (selected-window) delta t)) nil
      (window-resize (selected-window) delta t))
    delta))

(global-set-key (kbd "C-x w") 'fit-window-to-buffer-horizontally)

(provide 'widescreen)

;;; widescreen.el ends here
