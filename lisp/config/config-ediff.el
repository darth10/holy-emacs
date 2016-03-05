;;; Configuration for ediff

(use-package ediff
  :bind (("C-! +" . ediff)
         ("C-! =" . ediff-buffers)
         ("C-! C-+" . ediff)
         ("C-! C-=" . ediff-buffers)
         ("C-x <f10>" . ediff-buffers)
         ("C-x S-<f10>" . ediff)
         ("C-: <f10>" . vc-ediff)
         ("C-: C-=" . vc-ediff)
         :map ediff-mode-map
         ("M-<down>" . ediff-next-difference)
         ("M-<up>" . ediff-previous-difference)
         ("M-<right>" . ediff-copy-A-to-B)
         ("M-<left>" . ediff-copy-B-to-A))
  :config
  (custom-set-variables
   '(ediff-split-window-function (quote split-window-horizontally))
   '(ediff-window-setup-function (quote ediff-setup-windows-plain)))
  (ediff-toggle-skip-similar))

(provide 'config-ediff)
