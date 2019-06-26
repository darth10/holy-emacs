;;; Configuration for Haskell

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode))
  :lang (:map haskell-mode-map
         (:repl-start . intero-repl)
         (:load-file . intero-repl-load))
  :config
  (use-package exec-path-from-shell
    :defer 2
    :unless (core:is-windows-p)
    :config
    (exec-path-from-shell-copy-env "STACK_ROOT"))

  (use-package intero
    :ensure t
    :config
    (add-hook 'haskell-mode-hook 'intero-mode)))

(provide 'mod-lang-haskell)
