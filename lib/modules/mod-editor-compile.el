;;; mod-editor-compile.el --- Editor packages for compilation   -*- lexical-binding: t; -*-

(use-package compile
  :defer 2
  :bind (("C-! k" . compile)
         ("C-! C-k" . compile)
         ("C-x <f5>" . compile)))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode))

(provide 'mod-editor-compile)
