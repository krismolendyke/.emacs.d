(require 'smex)
(smex-initialize)

;; Replace execute-extended-command binding with smex.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Keep execute-extended-command at hand just in case.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Share smex history across my machines.
(setq smex-save-file (expand-file-name ".smex-items" dropbox-directory))

(provide 'setup-smex)
