;;; Common configuration

(require 'highlight-sexps)

(defmacro defconfig (name &rest body)
  `(defun ,name ()
     (local-set-key "\r" 'newline-and-indent)
     ,@body))

(defmacro global-for-key (key &rest body)
  `(global-set-key
    ,key
    (lambda ()
      (interactive)
      ,@body)))

(defun is-windows? ()
  (equal system-type 'windows-nt))

(defun configure-lisp ()
  (paredit-mode)
  ;; uncomment for sexp highlighting by default
  ;; (highlight-sexps-mode t)
  (local-set-key (kbd "C-<f12>") 'highlight-sexps-mode))

(provide 'config-common)
