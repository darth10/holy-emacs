;;; mod-editor-format.el --- Editor packages for code formatting  -*- lexical-binding: t; -*-

(use-package wide-column)

(use-package editorconfig
  :defer 2
  :config
  (editorconfig-mode 1))

(use-package ws-butler
  :commands (ws-butler-mode)
  :hook ((prog-mode conf-mode) . ws-butler-mode)
  :bind (("C-' d" . ws-butler-mode)
         ("C-' C-d" . ws-butler-mode)))

(provide 'mod-editor-format)
