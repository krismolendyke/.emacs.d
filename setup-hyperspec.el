;; Set HyperSpec root in Dropbox.
(setq common-lisp-hyperspec-root
      (format "file://%s/"
              (expand-file-name "Documents/HyperSpec" dropbox-directory)))

(provide 'setup-hyperspec)
