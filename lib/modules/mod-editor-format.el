;;; mod-editor-format.el --- Editor packages for code formatting  -*- lexical-binding: t; -*-

(use-package wide-column
  :ensure t)

(use-package editorconfig
  :ensure t
  :defer 2
  :config
  (editorconfig-mode 1))

(use-package ws-butler
  :ensure t
  :bind (("C-' d" . ws-butler-mode)
         ("C-' C-d" . ws-butler-mode))
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'conf-mode-hook #'ws-butler-mode))

(provide 'mod-editor-format)
