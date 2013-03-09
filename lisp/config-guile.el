;;; Configuration for Guile

(require 'geiser)

(defun guile-load-and-repl ()
  (interactive)
  (geiser-compile-current-buffer)
  (geiser-mode-switch-to-repl t))

(defun configure-guile ()
  (paredit-mode)
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key (kbd "C-x <f10>") 'run-guile)
  (local-set-key (kbd "C-x <f5>") 'guile-load-and-repl)
  (local-set-key (kbd "C-x <f8>") 'connect-to-guile))

(add-hook 'scheme-mode-hook 'configure-guile)

(provide 'config-guile)
