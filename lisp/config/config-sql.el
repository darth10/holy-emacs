;;; Configuration for SQL

(require 'config-common)

(defconfig configure-sql
  (auto-complete-mode)
  (smartparens-mode)
  (sql-set-product 'ms))

(defun configure-sql-interactive ()
  (auto-complete-mode)
  (setq yas-extra-modes '(sql-mode)))

(add-hook 'sql-mode-hook 'configure-sql)
(add-hook 'sql-interactive-mode-hook 'configure-sql-interactive)

(provide 'config-sql)
