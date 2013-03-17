;;; Configuration for Guile

(require 'geiser)

(defun guile-load-and-repl ()
  (interactive)
  (geiser-compile-current-buffer)
  (geiser-mode-switch-to-repl t))

(defun configure-guile-paredit ()
  (paredit-mode))

(defun configure-guile ()
  (configure-guile-paredit)
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key (kbd "C-<f10>") 'run-guile)
  (local-set-key (kbd "C-<f5>") 'guile-load-and-repl)
  (local-set-key (kbd "C-<f8>") 'connect-to-guile))

(defun configure-guile-inf ()
  (when (eq major-mode 'geiser-repl-mode)
    (configure-guile-paredit)))

(add-hook 'scheme-mode-hook 'configure-guile)
(add-hook 'comint-mode-hook 'configure-guile-inf)

(provide 'config-guile)
