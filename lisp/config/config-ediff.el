;;; Configuration for ediff

(defun configure-ediff ()
  (setq ediff-split-window-function 'split-window-horizontally))

(defun configure-ediff-bindings ()
  (ediff-toggle-skip-similar)
  (local-set-key (kbd "M-<down>") 'ediff-next-difference)
  (local-set-key (kbd "M-<up>") 'ediff-previous-difference)
  (local-set-key (kbd "M-<right>") 'ediff-copy-A-to-B)
  (local-set-key (kbd "M-<left>") 'ediff-copy-B-to-A))

(add-hook 'ediff-mode-hook 'configure-ediff)
(add-hook 'ediff-startup-hook 'configure-ediff-bindings)

(provide 'config-ediff)
