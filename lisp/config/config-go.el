;;; Configuration for Go

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'config-go)
