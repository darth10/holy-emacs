;;; mod-lang-scheme.el --- Configuration for Scheme  -*- lexical-binding: t; -*-

(use-package scheme
  :straight nil
  :mode (("\\.scm\\'" . scheme-mode)
         ("\\.rkt\\'" . scheme-mode))
  :hook ((scheme-mode . paredit-mode)
         (scheme-mode . +scheme--highlight-sexp-setup))
  :init
  (defun +scheme--highlight-sexp-setup ()
    (+highlight-sexp:bind-keys 'scheme-mode-map)))

(use-package geiser
  :hook (geiser-repl-mode . paredit-mode)
  :lang (:map scheme-mode-map
         (:repl-start . run-geiser)
         (:repl-connect . geiser-connect)
         :map geiser-mode-map
         (:find-definition . geiser-edit-symbol-at-point)
         (:eval-buffer . +scheme/load-and-repl)
         (:load-file . +scheme/load-current-file))
  :config
  (defun +scheme/load-and-repl ()
    (interactive)
    (geiser-compile-current-buffer)
    (geiser-mode-switch-to-repl t))

  (defun +scheme/load-current-file ()
    (interactive)
    (geiser-load-file buffer-file-name)))

(provide 'mod-lang-scheme)
