;;; Configuration for Emacs Lisp

(use-package lisp-mode
  :config
  (bind-key "C-x a a" 'eval-print-last-sexp lisp-interaction-mode-map)
  (bind-key "C-x C-a C-a" 'eval-print-last-sexp lisp-interaction-mode-map)
  (bind-key "C-<f5>" 'eval-print-last-sexp lisp-interaction-mode-map)
  (bind-key "C-c l" 'load-file emacs-lisp-mode-map)
  (bind-key "C-c C-l" 'load-file emacs-lisp-mode-map)
  (bind-key "C-x a a" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-x C-a C-a" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-<f5>" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-<f10>" 'core/find-or-run-eshell emacs-lisp-mode-map)
  (bind-key "C-! C-r" 'core/find-or-run-eshell emacs-lisp-mode-map)

  (defun +elisp-compile-file ()
	(interactive)
	(core:compile-file buffer-file-name))

  (bind-key "C-c k" '+elisp-compile-file emacs-lisp-mode-map)
  (bind-key "C-c C-k" '+elisp-compile-file emacs-lisp-mode-map))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eshell-mode-hook 'eldoc-mode))

(use-package esup
  :ensure t
  :defer 2)

(use-package try
  :ensure t
  :defer 2)

(provide 'config-elisp)
