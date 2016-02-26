;;; Configuration for search

;;; occur
(use-package helm
  :bind ("M-s i" . helm-occur))

;;; grep
(use-package emacs/grep
  :ensure package
  :bind (("M-s G" . grep)
         ("M-s g" . rgrep)))

;;; vc-git-grep
(use-package emacs/vc-git
  :ensure package
  :bind (("C-: <f3>" . vc-git-grep)
         ("M-s :" . vc-git-grep)
         ("C-: M-s" . vc-git-grep)))

;;; ag
(use-package ag
  :bind (("C-<f3>" . ag)
         ("M-s s" . ag)
         ("C-S-<f3>" . ag-regexp)
         ("M-s r" . ag-regexp)
         ("M-s a s" . ag-project)
         ("M-s a r" . ag-project-regexp)))

(provide 'config-search)
