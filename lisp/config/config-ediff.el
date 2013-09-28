;;; Configuration for ediff

(defun configure-ediff ()
  (setq ediff-split-window-function 'split-window-horizontally))

(defun configure-ediff-bindings ()
  (ediff-toggle-skip-similar)
  (local-set-key (kbd "<f7>") 'ediff-next-difference)
  (local-set-key (kbd "S-<f7>") 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'configure-ediff)
(add-hook 'ediff-startup-hook 'configure-ediff-bindings)

(provide 'config-ediff)
