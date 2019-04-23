;;; Configuration for SQL

(use-package sql
  :bind (:map sql-mode-map
         ("C-! C-r" . sql-product-interactive)
         ("C-<f10>" . sql-product-interactive)
         ("C-x a a" . sql-send-paragraph)
         ("C-x C-a C-a" . sql-send-paragraph)
         ("C-c d" . sql-set-product)
         ("C-c C-d" . sql-set-product))
  :config
  (add-to-list 'process-coding-system-alist '("sqlcmd" . cp850-dos))
  (setq sql-ms-program "sqlcmd")
  (setq sql-ms-options nil)

  (defun configure-sql ()
    (sql-set-product 'ms))

  (defun configure-sql-interactive ()
    (toggle-truncate-lines t)
    (setq yas-extra-modes '(sql-mode)))

  (add-hook 'sql-mode-hook 'configure-sql)
  (add-hook 'sql-interactive-mode-hook 'configure-sql-interactive))

(provide 'mod-lang-sql)
