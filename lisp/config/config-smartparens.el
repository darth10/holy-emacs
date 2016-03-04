;;; Configuration for smartparens

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (use-package cc-mode :config (add-hook 'c-mode-hook 'smartparens-mode))
  (use-package python :config (add-hook 'python-mode-hook 'smartparens-mode))
  (use-package php-mode :config (add-hook 'php-mode-hook 'smartparens-mode))
  (use-package ruby-mode :config (add-hook 'ruby-mode-hook 'smartparens-mode)))

(provide 'config-smartparens)
