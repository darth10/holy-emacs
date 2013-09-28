;;; Configuration for SQL

(require 'config-common)

(defconfig configure-sql
  (auto-complete-mode))

(add-hook 'sql-mode-hook 'configure-sql)

(provide 'config-sql)
