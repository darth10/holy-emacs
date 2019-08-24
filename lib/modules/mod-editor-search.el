;;; mod-editor-search.el --- Editor packages for search  -*- lexical-binding: t; -*-

(use-package anzu
  :ensure t
  :defer t
  :hook (after-init . global-anzu-mode))

(use-package findr
  :ensure t
  :defer 2)

(use-package grep
  :bind (("M-s G" . grep)
         ("M-s M-G" . grep)
         ("M-s g" . rgrep)
         ("M-s M-g" . rgrep)))

(use-package vc-git
  :after magit
  :bind (("M-s :" . vc-git-grep)
         ("M-s M-:" . vc-git-grep)
         ("C-: <f3>" . vc-git-grep)
         ("C-: M-s" . vc-git-grep)))

(use-package ag
  :ensure t
  :bind (("M-s a" . ag)
         ("M-s M-a" . ag)
         ("M-s e" . ag-regexp)
         ("M-s e" . ag-regexp)
         ("M-s p" . ag-project)
         ("M-s M-p" . ag-project)
         ("M-s P" . ag-project-regexp)
         ("M-s M-P" . ag-project-regexp)))

(use-package isearch
  :bind (("M-s s" . isearch-forward)
         ("M-s M-s" . isearch-forward)
         ("M-s r" . isearch-backward)
         ("M-s M-r" . isearch-backward)
         :map isearch-mode-map
         ("<f3>" . isearch-repeat-forward)
         ("S-<f3>" . isearch-repeat-backward)))

(provide 'mod-editor-search)
