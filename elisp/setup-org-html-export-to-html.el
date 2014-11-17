;;; setup-org-html-export-to-html.el --- Setup HTML export for org-mode.

;;; Commentary:
;;
;; This file contains the code that I would like to be run before
;; `org-html-export-to-html' is run.  It also needs to be able to be
;; run from an Emacs batch process so that documentation can be
;; exported via git commit hooks.

;;; Code:

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

(require 'htmlize)
(append-to-file (format "htmlize-version: %s\n" htmlize-version) nil "/tmp/emacs.out")

(require 'org)
(require 'ox-html)
(append-to-file (format "org-version: %s\n" org-version) nil "/tmp/emacs.out")

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)

(setq org-html-head-include-default-style nil
      org-html-postamble-format '(("en" "<p class=\"author\">%a</p>
<p class=\"date\">%C</p>
<p class=\"creator\">%c</p>"))
      org-html-postamble t)

(defun update-org-css ()
  "Update `org-html-head' with custom CSS.
See ~/.emacs.d/org.css."
  (let ((css-filename (expand-file-name "org.css" user-emacs-directory))
        (css-wrapper "<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
%s/*]]>*/-->
</style>"))
    (setq org-html-head (format css-wrapper
                                (with-temp-buffer
                                  (insert-file-contents css-filename)
                                  (buffer-string))))))

(add-hook 'org-export-before-processing-hook
          (lambda (backend) (update-org-css)))

(setq org-html-head-extra
      "<link href='http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css' rel='stylesheet'>")

;;; setup-org-html-export-to-html.el ends here
