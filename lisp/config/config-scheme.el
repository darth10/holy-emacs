;;; Configuration for Guile

(require 'config-common)

(defun scheme-load-and-repl ()
  (interactive)
  (geiser-compile-current-buffer)
  (geiser-mode-switch-to-repl t))

(defconfig configure-scheme
  (require 'geiser)
  (local-set-key (kbd "C-<f10>") 'run-geiser)
  (local-set-key (kbd "C-<f5>") 'scheme-load-and-repl))

(defun configure-scheme-inf ()
  (when (eq major-mode 'geiser-repl-mode)
    (configure-lisp)))

(add-hook 'scheme-mode-hook 'configure-scheme)
(add-hook 'scheme-mode-hook 'configure-lisp)
(add-hook 'comint-mode-hook 'configure-scheme-inf)

(provide 'config-scheme)
