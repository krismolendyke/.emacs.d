(require 'git-commit-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(provide 'setup-git)
