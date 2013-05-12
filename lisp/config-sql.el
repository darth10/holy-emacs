;;; Configuration for SQL

(defun configure-sql ()
  (auto-complete-mode)
  (local-set-key "\r" 'newline-and-indent))

(add-hook 'sql-mode-hook 'configure-sql)

(provide 'config-sql)
