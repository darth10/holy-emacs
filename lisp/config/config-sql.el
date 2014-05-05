;;; Configuration for SQL

(require 'config-common)

(defconfig configure-sql
  (auto-complete-mode)
  (sql-set-product 'ms))

(add-hook 'sql-mode-hook 'configure-sql)

(provide 'config-sql)
