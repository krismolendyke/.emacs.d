(load-theme 'wombat t)
;(load-theme 'whiteboard t)

;; Consolas on OS X, please!
(if (equal system-type 'darwin)
    (set-frame-font
     "-apple-Consolas-medium-normal-normal-*-18-*-*-*-m-0-fontset-auto1"))

(defun get-max-rows (pixel-height)
  "Return the maximum number of rows that will fit with this screen.
Given a screen pixel height at the current frame character height, calculate
the maximum number of rows that will fit with that height."
  (if (window-system)
      (/ pixel-height (frame-char-height))))

(defun set-frame-height-to-max ()
  "Set the selected frame height to the maximum that will fit the current
screen resolution."
  (if (window-system)
      (set-frame-height (selected-frame)
			(get-max-rows (- (display-pixel-height) 44)))))

(set-frame-height-to-max)

(provide 'appearance)
