;;; Common configuration

(require 'highlight-sexps)

(defmacro defconfig (name &rest body)
  `(defun ,name ()
     (local-set-key "\r" 'newline-and-indent)
     ,@body))

(defun configure-lisp ()
  (paredit-mode)
  (highlight-sexps-mode t)
  (local-set-key (kbd "C-<f12>") 'highlight-sexps-mode))

(provide 'config-common)
