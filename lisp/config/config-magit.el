;;; Configuration for magit

(defun git-diff-tree ()
  (interactive)
  (magit-diff-working-tree "HEAD")
  (other-window 1))

(defun configure-magit-status-mode ()
  (local-unset-key (kbd "x"))
  (local-unset-key (kbd ":")))

(setq magit-auto-revert-mode t)
(add-hook 'magit-status-mode-hook 'configure-magit-status-mode)

(provide 'config-magit)
