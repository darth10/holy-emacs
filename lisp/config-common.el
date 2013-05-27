;;; Common configuration

(require 'highlight-sexps)

(defun configure-lisp ()
  (paredit-mode)
  (highlight-sexps-mode t)
  (local-set-key (kbd "C-<f12>") 'highlight-sexps-mode))

(provide 'config-common)
