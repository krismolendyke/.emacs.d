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

(defun set-font (font-alist)
  "Set the font family and size to the given font alist of the
format (family . point)."
  (set-frame-font (car font-alist))
  (set-face-attribute 'default nil :height (* 10 (cdr font-alist))))

(defun set-font-from-list (l)
  "Set the font to first available font alist in the given list."
  (if (null l) nil
    (set-font (car l))
    (if (string= (caar l) (face-attribute 'default :family (selected-frame)))
        (caar l)
      (set-font-from-list (cdr l)))))

;; Ordered list of preferred fonts and sizes.
(set-font-from-list
 '(("Source_Code_Pro" . 15)
   ("Consolas" . 18)
   ("Ubuntu_Mono" . 17)
   ("Inconsolata" . 18)))

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
