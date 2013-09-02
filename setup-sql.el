(add-hook 'sql-mode-hook (lambda ()
                           (setq sql-product 'mysql)
                           (sql-highlight-mysql-keywords)))

(provide 'setup-sql)
