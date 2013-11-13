;;; Configuration for Dired mode

(require 'direx)

(defun configure-dired ()
  (local-set-key (kbd "C-x C-/") 'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'configure-dired)
(global-set-key (kbd "C-c C-j") 'direx:jump-to-directory)

(provide 'config-dired)
