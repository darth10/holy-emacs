;;; Configuration for Emacs Lisp

(defun split-and-ielm ()
  (interactive)
  (split-window-right)
  (ielm))

(defun configure-elisp ()
  (paredit-mode)
  (turn-on-eldoc-mode)
  (local-set-key (kbd "C-x <f10>") 'split-and-ielm)
  (local-set-key (kbd "C-x <f5>") 'eval-buffer))

(defun configure-elisp-inf ()
  (local-set-key (kbd "C-x <f5>") 'eval-print-last-sexp))

(add-hook 'emacs-lisp-mode-hook 'configure-elisp)
(add-hook 'lisp-interaction-mode-hook 'configure-elisp-inf)

(provide 'config-elisp)
