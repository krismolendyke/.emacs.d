(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(add-to-list 'auto-mode-alist '("^\\.gitconfig$" . conf-mode))

(provide 'setup-git)
