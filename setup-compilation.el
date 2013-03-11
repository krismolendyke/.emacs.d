(add-hook 'compilation-mode-hook (lambda ()
                                   (set-face-foreground 'compilation-error "tomato1")))

(provide 'setup-compilation)
