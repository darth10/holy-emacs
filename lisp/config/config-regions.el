;;; Configuration for expand-region, regions

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package regions
  :load-path "lisp/lib/"
  :bind (("M-<down>" . move-line-region-down)
         ("M-<up>" . move-line-region-up)
         ("M-n" . move-line-region-down)
         ("M-p" . move-line-region-up)))

(provide 'config-regions)
