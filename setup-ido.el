(require 'flx-ido)
(require 'ido)
(require 'ido-vertical-mode)

(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Boring arrows be gone!
(setq ido-vertical-decorations '("\n    "  ; left bracket around prospect list
                                 ""        ; right bracket around prospect list
                                 "\n    "  ; separator between prospects, depends on `ido-separator`
                                 "\n    ▼" ; inserted at the end of a truncated list of prospects
                                 "["       ; left bracket around common match string
                                 "]"       ; right bracket around common match string
                                 " ✘"      ; no match
                                 " ✔"      ; matched
                                 " [Not readable]"
                                 " [Too big]"
                                 " ?"      ; confirm
                                 "\n    "  ; left bracket around the sole remaining completion
                                 " ✔"      ; right bracket around the sole remaining completion
                                 ))

(custom-set-faces '(ido-first-match ((t (:foreground "green4"))))
                  '(ido-only-match ((t (:foreground "green4"))))
                  '(flx-highlight-face ((t (:foreground "green3" :underline nil)))))

(add-hook 'ido-minibuffer-setup-hook
          #'(lambda ()
              "Bump up minibuffer text size and height."
              (text-scale-set 3)
              (setq max-mini-window-height 20)))

;; All this to avoid `ido-vertical-mode' from eating M-p
(setq ido-vertical-define-keys nil)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
            (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
            (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match)))

(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)

(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-show-dot-for-dired t
      ido-max-file-prompt-width 0.2
      ido-use-faces t
      flx-ido-use-faces t)

(provide 'setup-ido)
