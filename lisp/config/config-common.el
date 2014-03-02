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

(defun config-set-hl-sexp (to-set-hl-sexp)
  (let ((to-set-hl-line   (if highlight-sexps-mode t -1))
        (to-set-hl-sexp (if highlight-sexps-mode -1 t)))
    (progn
      (hl-line-mode to-set-hl-line)
      (highlight-sexps-mode to-set-hl-sexp))))

(defun config-toggle-hl-sexp ()
  (interactive)
  (config-set-hl-sexp highlight-sexps-mode))

(defun configure-lisp ()
  (paredit-mode)
  (config-set-hl-sexp t)
  (local-set-key (kbd "C-<f12>") 'config-toggle-hl-sexp)
  (local-set-key (kbd "C-' C-s") 'config-toggle-hl-sexp))

(provide 'config-common)
