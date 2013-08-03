;;; Configuration for Dired mode

(defun configure-dired ()
  (local-set-key (kbd "C-x C-/") 'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'configure-dired)

(provide 'config-dired)
