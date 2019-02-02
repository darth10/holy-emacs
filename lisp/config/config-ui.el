;;; Configuration for UI/theme -*- lexical-binding: t; -*-

(use-package hl-line
  :bind (("C-' l" . hl-line-mode)
         ("C-' C-l" . hl-line-mode)
         ("C-<f4>" . hl-line-mode))
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'org-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(use-package highlight-symbol
  :ensure t
  :bind (("C-' ." . highlight-symbol-mode)
         ("C-' C-." . highlight-symbol-mode))
  :commands (highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'conf-mode-hook #'highlight-symbol-mode))

(provide 'config-ui)
