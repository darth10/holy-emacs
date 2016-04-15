;;; Configuration for Haskell

(use-package haskell-mode
  :ensure t
  :config

  (defun load-file-in-inf-haskell ()
    (interactive)
    (inferior-haskell-load-file)
    (switch-to-haskell))

  (bind-key "C-<f10>" 'switch-to-haskell haskell-mode-map)
  (bind-key "C-! C-r" 'switch-to-haskell haskell-mode-map)
  (bind-key "C-<f5>" 'load-file-in-inf-haskell haskell-mode-map)
  (bind-key "C-x C-a C-a" 'load-file-in-inf-haskell haskell-mode-map)
  (bind-key "C-x a a" 'load-file-in-inf-haskell haskell-mode-map)

  (add-hook 'haskell-mode-hook 'haskell-indent-mode)

  (use-package haskell-doc
    :diminish haskell-doc-mode
    :config
    (add-hook 'haskell-mode-hook 'haskell-doc-mode))

  (use-package haskell-cabal
    :config
    (add-hook 'haskell-cabal-mode-hook 'configure-haskell-newline-indent)))

(use-package ghc
  :ensure t
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package company-ghc
  :ensure t
  :config
  (push 'company-ghc company-backends))

(provide 'config-haskell)
