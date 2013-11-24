;;; im-feeling-lucky.el --- Google I'm Feeling Lucky on active region.

;;; Commentary:
;; Open a browser with the result of doing a Google I'm Feeling Lucky
;; search with the current region as the query.
;;
;; Inspired by:
;; https://github.com/timvisher/.emacs.d/blob/master/site-lisp/google-lucky.el

(require 'browse-url)
(require 'url)

;;; Code:

(defvar ifl--url "https://encrypted.google.com/search?btnI"
  "The Google I'm Feeling Lucky URL.
The 'q' query string parameter should be omitted.")

(defvar ifl--query-key "q"
  "The query string parameter key for the query.")

(defun ifl--build-query-string (urlobj query)
  "Build a query string from a `URLOBJ' and a `QUERY'."
  (url-build-query-string
   (cons (list ifl--query-key query)
         (url-parse-query-string (cdr (url-path-and-query urlobj))))))

(defun ifl--file (urlobj query)
  "Return a file string from a `URLOBJ' and a `QUERY'."
  (let ((path-and-query (cons (car (url-path-and-query urlobj))
                              (ifl--build-query-string urlobj query))))
    (format "%s?%s" (car path-and-query) (cdr path-and-query))))

(defun ifl--url (url query)
  "Return a string of a `URL' containing a `QUERY'."
  (let* ((urlobj (url-generic-parse-url url))
         (type (url-type urlobj))
	 (user (url-user urlobj))
	 (pass (url-password urlobj))
	 (host (url-host urlobj))
	 (port (url-port-if-non-default urlobj))
         (file (ifl--file urlobj query))
         (frag (url-target urlobj)))
    (url-recreate-url (url-parse-make-urlobj type user pass host port file frag
                                             nil t))))

(defun ifl--send-query-to-browser (url query)
  "Open a browser at the location made from a `URL' and a `QUERY'."
  (browse-url (ifl--url url query)))

(defun ifl-query (query)
  "Open an I'm Feeling Lucky result for a QUERY."
  (interactive "sI'm Feeling Lucky: ")
  (ifl--send-query-to-browser ifl--url query))

(defun ifl-region-or-query (begin end)
  "Open an I'm Feeling Lucky result for a region or query.
Argument BEGIN The beginning of the region to use as a query.
Argument END The end of the region to use as a query."
  (interactive "r")
  (if (use-region-p)
      (ifl--send-query-to-browser ifl--url
                                  (buffer-substring-no-properties begin end))
    (call-interactively 'ifl-query)))

(provide 'im-feeling-lucky)

;;; im-feeling-lucky.el ends here