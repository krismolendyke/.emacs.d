;; Add themes.
(dolist
    (theme (directory-files(expand-file-name "themes" user-emacs-directory) t "\\w+"))
  (when (file-directory-p theme)
    (add-to-list 'custom-theme-load-path theme)))

;; Tomorrow as a submodule.  It has a bunch of other editor support.
(add-to-list 'custom-theme-load-path
             (expand-file-name
              "themes/tomorrow/GNU Emacs" user-emacs-directory))

(load-theme 'tomorrow-night)
;(load-theme 'wombat t)
;(load-theme 'zenburn t)
;(load-theme 'whiteboard t)

;; Skinny bar default cursor instead of filled box.
(set-default 'cursor-type '(bar . 1))

(defun set-font (name point)
  "Set the font to the given font name and point size."
  (set-frame-font name)
  (set-face-attribute 'default nil :height (* 10 point)))

;; (set-font "Consolas" 18)
(set-font "Source_Code_Pro" 15)
;; (set-font "Ubuntu_Mono" 17)
;; (set-font "Inconsolata" 18)

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
