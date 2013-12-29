;;; pre-commit.el --- git pre-commit hook code

;;; Commentary:
;;
;; This file contains the code that I would like to be run at git
;; pre-commit time.  See .git/hooks/pre-commit.

;;; Code:

(require 'org)
(require 'ox-html)

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

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)

(setq org-html-head-include-default-style nil
      org-html-postamble-format '(("en" "<p class=\"author\">%a</p>
<p class=\"date\">%C</p>
<p class=\"creator\">%c</p>"))
      org-html-postamble t)

(defun update-org-css ()
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

(add-hook 'org-export-before-processing-hook
          (lambda (backend) (update-org-css)))

(setq org-html-head-extra
      "<link href='http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css' rel='stylesheet'>")

;;; pre-commit.el ends here
