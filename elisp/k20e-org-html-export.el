;;; k20e-org-html-export.el --- Setup HTML export for org-mode.

;;; Commentary:
;;
;; This file contains the code that I would like to be run before
;; `org-html-export-to-html' is run.  It also needs to be able to be
;; run from an Emacs batch process so that documentation can be
;; exported via git commit hooks.

;;; Code:

;; Add the version of org-mode I use to load-path.
(add-to-list
 'load-path
 (expand-file-name
  "lisp"
  (expand-file-name
   "contrib"
   (expand-file-name
    "org-mode"
    (expand-file-name
     "site-lisp"
     user-emacs-directory)))))

;;; htmlize has been moved out of org and now Cask is responsible for
;;; installing the dependency.
;;; TODO should probably just init cask or whatever
(add-to-list
 'load-path
 (expand-file-name
  "elpa"
  (expand-file-name
   "27.0"
   (expand-file-name
    ".cask"
    user-emacs-directory))))

;; Then use that version of org-mode, etc.
(require 'org)
(require 'ox-html)

(defvar k20e/org-html-head-extra
  "<link href='https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css' rel='stylesheet'>
<link href='https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono' rel='stylesheet'>"
  "Extra HTML to add to the <head> tag.")

;; Fontify _SRC blocks in org-mode buffers.
(setq org-src-fontify-natively t)

;; Generate a stylesheet rather than inline CSS.
(setq org-html-htmlize-output-type 'css)

;; Customize HTML export styling.
(setq org-html-head-include-default-style nil
      org-html-postamble-format '(("en" "<p class=\"author\">%a</p>
<p class=\"date\">%C</p>
<p class=\"creator\">%c</p>"))
      org-html-postamble t)

(defun k20e/update-org-css ()
  "Update the `org-html-head' variable with the contents of the
~/.emacs.d/org.css file."
  (let ((css-filename (expand-file-name "org.css" user-emacs-directory))
        (css-wrapper "<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
%s/*]]>*/-->
</style>"))
    (setq org-html-head (format css-wrapper
                                (with-temp-buffer
                                  (insert-file-contents css-filename)
                                  (buffer-string))))))

(defun k20e/org-export-before-processing-hook (backend)
  (k20e/update-org-css))

(add-hook 'org-export-before-processing-hook
          'k20e/org-export-before-processing-hook)

(setq org-html-head-extra k20e/org-html-head-extra)

(provide 'k20e-org-html-export)

;;; k20e-org-html-export.el ends here
