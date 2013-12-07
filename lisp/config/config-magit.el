;;; Configuration for magit

(defun git-diff-tree ()
  (interactive)
  (magit-diff-working-tree "HEAD")
  (other-window 1))

(defun git-show-gitk ()
  (interactive)
  (message "Waiting for gitk ...")
  (shell-command "gitk")
  (message "Exited gitk"))

(defun configure-magit-status-mode ()
  (local-unset-key (kbd "x")))

(add-hook 'magit-status-mode-hook 'configure-magit-status-mode)

(provide 'config-magit)
