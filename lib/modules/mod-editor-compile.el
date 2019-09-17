;;; mod-editor-compile.el --- Editor packages for compilation   -*- lexical-binding: t; -*-

(use-package compile
  :defer 2
  :bind (("C-! k" . compile)
         ("C-! C-k" . compile)
         ("C-x <f5>" . compile)))

(use-package flycheck
  :commands (flycheck-mode))

(use-package flycheck-pos-tip
  :commands (flycheck-pos-tip-mode)
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(provide 'mod-editor-compile)
