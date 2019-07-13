;;; mod-lang-elisp.el --- Configuration for Emacs Lisp  -*- lexical-binding: t; -*-

(use-package lisp-mode
  :lang (:map emacs-lisp-mode-map
         (:repl-start . core/find-or-run-eshell)
         (:eval-buffer . eval-buffer)
         (:load-file . load-file)
         (:compile-file . +elisp/compile-file)
         :map lisp-interaction-mode-map
         (:eval-buffer . eval-print-last-sexp))
  :config
  (defun +elisp/compile-file ()
	(interactive)
	(core:compile-file buffer-file-name)))

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

(provide 'mod-lang-elisp)
