;;; Configuration for Haskell

(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map
         ("C-<f10>" . haskell-interactive-switch)
         ("C-! C-r" . haskell-interactive-switch)
         ("C-c C-z" . haskell-interactive-switch)
         ("C-<f5>" . load-file-in-haskell-process)
         ("C-x C-a C-a" . load-file-in-haskell-process)
         ("C-x a a" . load-file-in-haskell-process)
         ("C-c C-l" . haskell-process-load-or-reload)
         ("C-c l" . haskell-process-load-or-reload)
         ("C-c C-k" . haskell-compile)
         ("C-c k" . haskell-compile)
         ("C-c C-n C-t" . haskell-process-do-type)
         ("C-c C-n C-i" . haskell-process-do-info)
         ("C-c C-n C-c" . haskell-process-cabal-build)
         ("C-c C-n c" . haskell-process-cabal))
  :config
  (custom-set-variables
   '(haskell-tags-on-save t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-show-debug-tips nil)
   '(haskell-compile-cabal-build-command "cd %s && stack build")
   '(haskell-process-type 'stack-ghci)
   '(haskell-process-path-ghci "stack")
   '(haskell-process-suggest-remove-import-lines t))

  ;; GHC 8.2.1+ workarounds
  (setq haskell-process-args-ghci
        '("-ferror-spans" "-fshow-loaded-modules"))
  (setq haskell-process-args-cabal-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-process-args-stack-ghci
        '("--ghci-options=-ferror-spans -fshow-loaded-modules"
          "--no-build" "--no-load"))
  (setq haskell-process-args-cabal-new-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  (defun load-file-in-haskell-process ()
    (interactive)
    (haskell-process-load-or-reload)
    (haskell-interactive-switch))

  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (use-package haskell-cabal
    :bind (:map haskell-cabal-mode-map
           ("C-c C-o" . 'haskell-process-cabal-build)
           ("C-c C-c" . 'haskell-process-cabal))
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
  :defer 4
  :config
  (push 'company-ghc company-backends)
  (setq company-ghc-show-info t))

(provide 'config-haskell)
