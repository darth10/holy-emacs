;;; config-search.el -*- lexical-binding: t; -*-

(use-package findr
  :ensure t
  :defer 2)

(use-package grep
  :bind (("M-s G" . grep)
         ("M-s g" . rgrep)))

(use-package vc-git
  :defer 2
  :bind (("C-: <f3>" . vc-git-grep)
         ("M-s :" . vc-git-grep)
         ("C-: M-s" . vc-git-grep)))

(use-package ag
  :ensure t
  :bind (("M-s a a" . ag)
         ("M-s a g" . ag-regexp)
         ("M-s a A" . ag-project)
         ("M-s a G" . ag-project-regexp)))

(use-package isearch
  :bind (("M-s s" . isearch-forward)
         ("M-s r" . isearch-backward)
         :map isearch-mode-map
         ("<f3>" . isearch-repeat-forward)
         ("S-<f3>" . isearch-repeat-backward)))

(provide 'mod-search)
