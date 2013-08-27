(require 'htmlize)
(require 'org)
(require 'org-publish)

;; Set the org directory.
(setq org-directory (expand-file-name "org" dropbox-directory))

;; MobileOrg setup.
(require 'org-mobile)
(setq org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory)
      org-mobile-directory (expand-file-name "Apps/MobileOrg" dropbox-directory))
(dolist (dir (dirs-in-dir org-directory '("." ".." ".git")))
  (add-to-list 'org-mobile-files dir))

;; Automatically insert a timestamp when a task is marked DONE.
(setq org-log-done t)

;; "Special" `C-a' and `C-e' movement in headlines.
(setq org-special-ctrl-a/e t)

;; Display entities as UTF-8 characters.
(add-hook 'org-mode-hook
          #'(lambda ()
              (org-toggle-pretty-entities)
              (visual-line-mode 0)))

;; org-capture.
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;; Global key binding to make storing links to files easier.
(global-set-key (kbd "C-c l") 'org-store-link)

;; Agenda files.
(setq org-agenda-files
      (list (expand-file-name "k20e.org" org-directory)
            (expand-file-name "work.org" org-directory)))

;; Enable "expert" export interface.
(setq org-export-dispatch-use-expert-ui t)

;; Enable Markdown export backend.
(require 'ox-md)
(add-to-list 'org-export-backends 'md)

;; Fontify _SRC blocks in org-mode buffers.
(setq org-src-fontify-natively t)

;; Generate a stylesheet rather than inline CSS.
(setq org-html-htmlize-output-type 'css)

;; Customize HTML export styling.
(require 'ox-html)
(setq org-html-head-include-default-style nil
      org-html-postamble-format '(("en" "<p class=\"author\">%a</p>
<p class=\"date\">%C</p>
<p class=\"creator\">%c</p>"))
      org-html-postamble t)

(defun update-org-css ()
  "Update the `org-html-head' variable with the contents of the
~/.emacs.d/org.css file."
  (interactive)
  (let ((css-filename (expand-file-name "org.css" user-emacs-directory))
        (css-wrapper "<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
%s/*]]>*/-->
</style>"))
    (setq org-html-head (format css-wrapper
                                (file-contents-as-string css-filename)))))
(add-hook 'org-export-before-processing-hook
          (lambda (backend) (update-org-css)))

;; Publishing.
;; (let* ((project-name "introduction-to-mathematical-thinking")
;;        (org-component (format "%s-files" project-name))
;;        (static-component (format "%s-static" project-name))
;;        (project-directory project-name)
;;        (root (expand-file-name project-directory org-directory))
;;        (public (expand-file-name "public" root))
;;        (static (expand-file-name "static" root)))
;;   (setq org-publish-project-alist
;;         `((,org-component
;;            :base-directory ,root
;;            :base-extension "org"
;;            :publishing-directory ,public
;;            :recursive t
;;            :publishing-function org-publish-org-to-html
;;            :headline-levels 6
;;            :auto-preamble t)
;;           (,static-component
;;            :base-directory ,static
;;            :base-extension "css\\|pdf"
;;            :publishing-directory ,public
;;            :recursive t
;;            :publishing-function org-publish-attachment)
;;           (,project-name :components (,org-component ,static-component)))))

(setq org-publish-project-alist
      `(("work"
         :base-directory ,(expand-file-name "work" org-directory)
         :base-extension "org"
         :publishing-directory ,(expand-file-name "published" (expand-file-name "work" org-directory))
         :publishing-function org-html-publish-to-html)))

;; Resolutions reminder.
(find-file (expand-file-name "2013-resolutions.org" org-directory))

(provide 'setup-org)
