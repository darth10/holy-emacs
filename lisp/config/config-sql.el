;;; Configuration for SQL

(require 'config-common)

(add-to-list 'process-coding-system-alist '("sqlcmd" . cp850-dos))
(setq sql-ms-program "sqlcmd")
(setq sql-ms-options nil)

(defconfig configure-sql
  ;; (auto-complete-mode)
  (smartparens-mode)
  (sql-set-product 'ms))

(defun configure-sql-interactive ()
  ;; (auto-complete-mode)
  (toggle-truncate-lines t)
  (setq yas-extra-modes '(sql-mode)))

(add-hook 'sql-mode-hook 'configure-sql)
(add-hook 'sql-interactive-mode-hook 'configure-sql-interactive)

(provide 'config-sql)
