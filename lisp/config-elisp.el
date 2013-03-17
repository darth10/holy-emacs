;;; Configuration for Emacs Lisp

(defun split-and-ielm ()
  (interactive)
  (split-window-right)
  (ielm))

(defun configure-elisp-paredit ()
  (paredit-mode)
  (turn-on-eldoc-mode))

(defun configure-elisp ()
  (configure-elisp-paredit)
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key (kbd "C-<f10>") 'split-and-ielm)
  (local-set-key (kbd "C-<f5>") 'eval-buffer))

(defun configure-elisp-inf ()
  (local-set-key (kbd "C-<f5>") 'eval-print-last-sexp))

(add-hook 'emacs-lisp-mode-hook 'configure-elisp)
(add-hook 'ielm-mode-hook 'configure-elisp-paredit)
(add-hook 'lisp-interaction-mode-hook 'configure-elisp-inf)

(provide 'config-elisp)
