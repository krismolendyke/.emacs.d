(require 'buffer-move)

(global-set-key (kbd "<H-S-up>") 'buf-move-up)
(global-set-key (kbd "<H-S-down>") 'buf-move-down)
(global-set-key (kbd "<H-S-left>") 'buf-move-left)
(global-set-key (kbd "<H-S-right>") 'buf-move-right)

(provide 'setup-buffer-move)
