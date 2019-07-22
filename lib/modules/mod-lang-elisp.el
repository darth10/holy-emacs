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

(use-package eval-sexp-fu
  :ensure t
  :commands (+eval-sexp-fu--init)
  :init
  (add-hook 'lisp-mode-hook #'+eval-sexp-fu--init)
  (add-hook 'emacs-lisp-mode-hook #'+eval-sexp-fu--init)
  (add-hook 'eshell-mode-hook #'+eval-sexp-fu--init)
  :config
  (face-spec-set 'eval-sexp-fu-flash '((t (:background "green" :foreground "black"))))
  (defun +eval-sexp-fu--init ()
    (require 'eval-sexp-fu)))

(use-package esup
  :ensure t
  :commands (esup))

(use-package try
  :ensure t
  :commands (try))

(provide 'mod-lang-elisp)
