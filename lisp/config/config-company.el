;;; Comfiguration for company

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("C-' a" . global-company-mode)
         ("C-' C-a" . global-company-mode))
  :config
  (global-unset-key (kbd "M-SPC"))
  (bind-key "M-SPC" 'company-manual-begin)
  (global-company-mode t))

(provide 'config-company)
