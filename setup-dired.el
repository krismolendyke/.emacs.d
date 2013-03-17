(add-hook 'dired-mode-hook
          #'(lambda ()
              (auto-revert-mode 1)
              (setq auto-revert-verbose nil)
              (set-face-foreground 'dired-flagged "tomato1")
              (set-face-attribute 'dired-flagged nil :strike-through t)))

;; C-x C-d is normally bound to `ido-list-directory' which I rarely need and
;; often type when I intend to run `ido-dired'.
(global-set-key (kbd "C-x C-d") 'ido-dired)

(provide 'setup-dired)
