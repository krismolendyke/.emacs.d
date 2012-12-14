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
  "Fit the selected window to the width of the longest line it contains."
  (interactive)
  (let* ((current-width (window-width))
         (longest-line (get-longest-line-length))
         (delta (* -1 (- current-width longest-line))))
    (if (zerop (window-resizable (selected-window) delta t)) nil
      (window-resize (selected-window) delta t))
    delta))

(global-set-key (kbd "C-x w") 'fit-window-to-buffer-horizontally)
