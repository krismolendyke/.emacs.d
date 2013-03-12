(require 'ido)

(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-show-dot-for-dired t
      ido-max-file-prompt-width 0.2)

(provide 'setup-ido)
