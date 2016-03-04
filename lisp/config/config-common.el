;;; Common configuration

(defmacro defconfig (name &rest body)
  `(defun ,name ()
     (local-set-key "\r" 'newline-and-indent)
     ,@body))

(defun is-windows? ()
  (equal system-type 'windows-nt))

;;; refactor into paredit use-package
;; (defun configure-lisp ()
;;   (paredit-mode))

(provide 'config-common)
