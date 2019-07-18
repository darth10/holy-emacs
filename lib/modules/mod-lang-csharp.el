;;; mod-lang-csharp.el --- Configuration for C#      -*- lexical-binding: t; -*-

;;; Install omnisharp server using omnisharp-install-server

(use-package csharp-mode
  :ensure t
  :mode (("\\.cs\\'" . csharp-mode))
  :lang (:map csharp-mode-map
         (:repl-start . +csharp/start-omnisharp-server))
  :config
  (defun +csharp/start-omnisharp-server ()
    (interactive)
    (omnisharp-start-omnisharp-server)
    (omnisharp-mode t)))

(use-package omnisharp
  :ensure t
  :after csharp-mode
  :commands (omnisharp-mode omnisharp-start-omnisharp-server)
  :lang (:map omnisharp-mode-map
         (:find-definition . omnisharp-go-to-definition)
         (:find-usages . omnisharp-find-usages)
         (:format-buffer . omnisharp-code-format-entire-file)
         (:compile-file . omnisharp-solution-errors))
  :bind (:map omnisharp-mode-map
         ("M-SPC" . company-omnisharp)
         ("." . +csharp/company-omnisharp-complete)
         ("C-." . omnisharp-run-code-action-refactoring)
         ("C--" . pop-tag-mark))
  :init
  (setq omnisharp-cache-directory
        (concat core-var-cache-dir-full-path "omnisharp/"))
  :config
  (defun +csharp/company-omnisharp-complete ()
    (interactive)
    (insert ".")
    (when omnisharp-mode
      (call-interactively 'company-omnisharp)))

  (add-hook 'omnisharp-mode-hook #'flycheck-mode)
  (add-hook 'csharp-mode-hook #'omnisharp-mode))

(use-package exec-path-from-shell
  :after csharp-mode
  :unless (core:is-windows-p)
  :config
  (message "exec-path-from-shell :config loaded")
  (exec-path-from-shell-copy-env "NUGET_PACKAGES"))

(provide 'mod-lang-csharp)
