;;; Configuration for diff-hl

(use-package diff-hl
  :ensure t
  :init
  (defun interactive-diff-hl-update ()
    (interactive)
    (diff-hl-update)
    (message "Refreshed diff-hl"))
  (bind-key "C-x r =" 'interactive-diff-hl-update)
  (global-diff-hl-mode 1))

(provide 'config-diff-hl)
