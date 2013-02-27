(add-hook 'sql-mode-hook (lambda ()
                           (setq sql-product 'mysql)))

(provide 'setup-sql)
