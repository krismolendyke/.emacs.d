(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(add-to-list 'auto-mode-alist '("^\\.gitconfig$" . conf-mode))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . text-mode))

(provide 'setup-git)
