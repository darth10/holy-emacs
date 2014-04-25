;;; Configuration for Dired mode


(require 'dired)
(require 'dired+)
(require 'dired-details)
(require 'dired-details+)
(require 'direx)

(defun configure-dired ()
  (hl-line-mode 1)
  (local-set-key (kbd "C-x C-/") 'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'configure-dired)

(global-unset-key (kbd "C-x C-j"))
(global-set-key (kbd "C-x C-j") 'dired-jump-other-window)
(global-set-key (kbd "C-c C-j") 'direx:jump-to-directory-other-window)

(provide 'config-dired)
