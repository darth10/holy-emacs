;;; Configuration for compile and flycheck

(use-package compile
  :bind (("C-! k" . compile)
         ("C-! C-k" . compile)
         ("C-x <f5>" . compile)
         ("M-<f5>" . recompile)
         ("C-x a k" . recompile)
         ("C-x C-a C-k" . recompile)))

(use-package flycheck
  :ensure t
  :defer 5)

(provide 'mod-compile)
