(require 'org)
(require 'org-publish)

;; Set the org directory.
(setq org-directory (expand-file-name "org" dropbox-directory))

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

;; Agenda files.
(setq org-agenda-files
      (list (expand-file-name "k20e.org" org-directory)
            (expand-file-name "work.org" org-directory)))

;; Publishing.
(let* ((project-name "introduction-to-mathematical-thinking")
       (org-component (format "%s-files" project-name))
       (static-component (format "%s-static" project-name))
       (project-directory project-name)
       (root (expand-file-name project-directory org-directory))
       (public (expand-file-name "public" root))
       (static (expand-file-name "static" root)))
  (setq org-publish-project-alist
        `((,org-component
           :base-directory ,root
           :base-extension "org"
           :publishing-directory ,public
           :recursive t
           :publishing-function org-publish-org-to-html
           :headline-levels 6
           :auto-preamble t)
          (,static-component
           :base-directory ,static
           :base-extension "css\\|pdf"
           :publishing-directory ,public
           :recursive t
           :publishing-function org-publish-attachment)
          (,project-name :components (,org-component ,static-component)))))

(provide 'setup-org)
