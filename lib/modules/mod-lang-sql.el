;;; mod-lang-sql.el --- Configuration for SQL        -*- lexical-binding: t; -*-

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :hook ((sql-mode . +sql/sql-product-setup)
         (sql-interactive-mode . +sql/sql-interactive-setup))
  :lang (:map sql-mode-map
         (:repl-start . sql-product-interactive)
         (:eval-buffer . sql-send-buffer))
  :config
  (add-to-list 'process-coding-system-alist '("sqlcmd" . cp850-dos))
  (setq sql-ms-program "sqlcmd")
  (setq sql-ms-options nil)

  (defun +sql/sql-product-setup ()
    (sql-set-product 'postgres))

  (defun +sql/sql-interactive-setup ()
    (toggle-truncate-lines t)
    (setq yas-extra-modes '(sql-mode))))

(provide 'mod-lang-sql)
