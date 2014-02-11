;;; Configuration for Haskell

(require 'config-common)
(require 'haskell-mode)
(require 'ghci-completion)

(defconfig configure-haskell-newline-indent)

(defun load-file-in-inf-haskell ()
  (interactive)
  (inferior-haskell-load-file)
  (switch-to-haskell))

(defun configure-haskell ()
  (turn-on-haskell-doc-mode)
  (haskell-indent-mode)
  (yas/minor-mode)
  (configure-haskell-newline-indent)
  (local-set-key (kbd "C-<f10>") 'switch-to-haskell)
  (local-set-key (kbd "C-<f5>") 'load-file-in-inf-haskell))

(defun configure-haskell-cabal ()
  (configure-haskell-newline-indent))

(defun configure-haskell-inf ()
  (ghci-completion-mode))

(add-hook 'haskell-mode-hook 'configure-haskell)
(add-hook 'inferior-haskell-mode-hook 'configure-haskell-inf)
(add-hook 'haskell-cabal-mode-hook 'configure-haskell-cabal)

(provide 'config-haskell)
