;;; Configuration for Dired mode

(use-package dired
  :config

  (use-package dired+
    :ensure t)

  (use-package direx
    :bind ("C-c C-j" . direx:jump-to-directory-other-window)
    :ensure t)

  (defun configure-dired ()
    (local-set-key (kbd "C-x C-/") 'wdired-change-to-wdired-mode))

  (add-hook 'dired-mode-hook 'configure-dired)
  (global-unset-key (kbd "C-x C-j"))
  (bind-key "C-x C-j" 'dired-jump-other-window))

(provide 'config-dired)
