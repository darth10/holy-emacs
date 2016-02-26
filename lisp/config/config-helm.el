;;; Configuration for Helm

(use-package helm
  :bind (("C-c ;" . helm-imenu)
         ("C-c C-;" . helm-imenu)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-~" . helm-mark-ring)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-<f2>" . helm-imenu)
         ("M-s i" . helm-occur)))

(use-package helm-swoop
  :bind ("M-]" . helm-swoop))

(provide 'config-helm)
