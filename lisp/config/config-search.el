;;; Configuration for search

;;; grep
(use-package emacs/grep
  :bind (("M-s G" . grep)
         ("M-s g" . rgrep)))

;;; vc-git-grep
(use-package emacs/vc-git
  :bind (("C-: <f3>" . vc-git-grep)
         ("M-s :" . vc-git-grep)
         ("C-: M-s" . vc-git-grep)))

;;; ag
(use-package ag
  :ensure t
  :bind (("C-<f3>" . ag)
         ("M-s s" . ag)
         ("C-S-<f3>" . ag-regexp)
         ("M-s r" . ag-regexp)
         ("M-s a s" . ag-project)
         ("M-s a r" . ag-project-regexp)))

(provide 'config-search)
