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
  :hook ((prog-mode conf-mode) . ws-butler-mode)
  :bind (("C-' d" . ws-butler-mode)
         ("C-' C-d" . ws-butler-mode))
  :commands (ws-butler-mode))

(provide 'mod-editor-format)
