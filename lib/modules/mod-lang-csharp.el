;;; mod-lang-csharp.el --- Configuration for C#      -*- lexical-binding: t; -*-

;;; Requires omnisharp server. Install it using M-x omnisharp-install-server.

(use-package csharp-mode
  :ensure t
  :mode (("\\.cs\\'" . csharp-mode))
  :lang (:map csharp-mode-map
         (:repl-start . +csharp/start-omnisharp-server)))

(use-package omnisharp
  :ensure t
  :after csharp-mode
  :commands (+csharp/start-omnisharp-server)
  :hook ((csharp-mode . omnisharp-mode)
         (omnisharp-mode . flycheck-mode))
  :lang (:comp (omnisharp-mode . company-omnisharp)
         :map omnisharp-mode-map
         (:find-definition . omnisharp-go-to-definition)
         (:find-usages . omnisharp-find-usages)
         (:format-buffer . omnisharp-code-format-entire-file)
         (:compile-file . omnisharp-solution-errors))
  :bind (:map omnisharp-mode-map
         ("C-." . omnisharp-run-code-action-refactoring))
  :init
  (setq omnisharp-cache-directory
        (concat core-var-cache-dir-full-path "omnisharp/"))
  :config
  (defun +csharp/start-omnisharp-server ()
    (interactive)
    (omnisharp-start-omnisharp-server)
    (omnisharp-mode t)))

(use-package exec-path-from-shell
  :after csharp-mode
  :unless (core:is-windows-p)
  :config
  (exec-path-from-shell-copy-env "NUGET_PACKAGES"))

(provide 'mod-lang-csharp)
