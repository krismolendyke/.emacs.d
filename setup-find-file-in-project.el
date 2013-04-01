(require 'find-file-in-project)

(add-to-list 'ffip-patterns "*.css" t)
(add-to-list 'ffip-patterns "*.soy" t)

(setq ffip-limit 8192
      ffip-find-options "-not -regex \".*/build.*\""
      ffip-full-paths t)

(global-set-key (kbd "C-x o") 'find-file-in-project)

(provide 'setup-find-file-in-project)
