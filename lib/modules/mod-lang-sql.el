;;; mod-lang-sql.el --- Configuration for SQL        -*- lexical-binding: t; -*-

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :lang (:map sql-mode-map
         (:repl-start . sql-product-interactive)
         (:eval-buffer . sql-send-buffer))
  :config
  (add-to-list 'process-coding-system-alist '("sqlcmd" . cp850-dos))
  (setq sql-ms-program "sqlcmd")
  (setq sql-ms-options nil)

  (defun +sql/configure-sql-product ()
    (sql-set-product 'postgres))

  (defun +sql/configure-sql-interactive ()
    (toggle-truncate-lines t)
    (setq yas-extra-modes '(sql-mode)))

  (add-hook 'sql-mode-hook #'+sql/configure-sql-product)
  (add-hook 'sql-interactive-mode-hook #'+sql/configure-sql-interactive))

(provide 'mod-lang-sql)
