;;; Configuration for smartparens

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.php\\'" . php-mode)
         ("\\.py\\'" . python-mode)))

(provide 'config-smartparens)
