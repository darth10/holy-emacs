;;; mod-lang-scheme.el --- Configuration for Scheme  -*- lexical-binding: t; -*-

(use-package geiser
  :ensure t
  :lang (:map scheme-mode-map
         (:repl-start . run-geiser)
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
