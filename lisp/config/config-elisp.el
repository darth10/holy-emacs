;;; Configuration for Emacs Lisp

(require 'config-common)
(require 'util)

(defconfig configure-elisp
  (turn-on-eldoc-mode)
  (local-set-key (kbd "C-<f10>") 'split-and-eshell)
  (local-set-key (kbd "C-<f5>") 'eval-buffer)
  (local-set-key (kbd "C-c C-k") 'eval-buffer))

(defun configure-elisp-inf ()
  (local-set-key (kbd "C-<f5>") 'eval-print-last-sexp))

(add-hook 'emacs-lisp-mode-hook 'configure-elisp)
(add-hook 'emacs-lisp-mode-hook 'configure-lisp)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'configure-elisp-inf)

(provide 'config-elisp)
