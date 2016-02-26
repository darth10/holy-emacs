;;; Configuration for Helm

(use-package helm
  :ensure t
  :bind (("C-c ;" . helm-imenu)
         ("C-c C-;" . helm-imenu)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-~" . helm-mark-ring)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-<f2>" . helm-imenu)
         ("M-s i" . helm-occur)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)))

(use-package helm-swoop
  :ensure t
  :bind ("M-]" . helm-swoop))

(use-package helm-ls-git
  :ensure t
  :bind ("C-: C-f" . helm-ls-git-ls))

(provide 'config-helm)
