;;; mod-lang-haskell.el --- Configuration for Haskell  -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)))

(use-package exec-path-from-shell
  :after haskell-mode
  :unless (core:is-windows-p)
  :config
  (exec-path-from-shell-copy-env "STACK_ROOT"))

(use-package intero
  :ensure t
  :after haskell-mode
  :hook (haskell-mode . intero-mode)
  :lang (:map haskell-mode-map
         (:repl-start . intero-repl)
         (:find-definition . intero-goto-definition)
         (:load-file . intero-repl-load)))

(provide 'mod-lang-haskell)
