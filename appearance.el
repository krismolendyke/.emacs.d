;; Add themes.
(dolist
    (theme (directory-files(expand-file-name "themes" user-emacs-directory) t "\\w+"))
  (when (file-directory-p theme)
    (add-to-list 'custom-theme-load-path theme)))

;; Tomorrow as a submodule.  It has a bunch of other editor support.
(add-to-list 'custom-theme-load-path
             (expand-file-name
              "themes/tomorrow/GNU Emacs" user-emacs-directory))

(defvar light-theme 'whiteboard "The default lightly colored theme.")
(defvar dark-theme 'tomorrow-night "The default darkly colored theme.")
;; Other good dark-theme candidates: wombat zenburn

(defun load-light-theme ()
  "Load a lightly colored theme for conditions when ambient light
is bright."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme light-theme t)
  (set-face-background 'hl-line "AntiqueWhite2"))

(defun load-dark-theme ()
  "Load a darkly colored theme for conditions when ambient light
is dark."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme dark-theme t)
  (set-face-background 'hl-line "gray17"))

(defun toggle-theme ()
  "Switch between the light and dark theme."
  (interactive)
  (if (member dark-theme custom-enabled-themes)
      (load-light-theme)
    (load-dark-theme)))

;; Load a dark theme by default.
(load-dark-theme)

;; For easy dark-light theme switching.
(global-set-key (kbd "C-x t") 'toggle-theme)

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
