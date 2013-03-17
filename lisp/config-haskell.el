;;; Configuration for Haskell

(require 'haskell-mode)
(require 'ghci-completion)

(defun haskell-newline-binding ()
  (local-set-key "\r" 'newline-and-indent))

(defun haskell-complete-binding ()
  (local-set-key (kbd "C-<tab>") 'dabbrev-expand))

(defun load-file-in-inf-haskell ()
  (interactive)
  (inferior-haskell-load-file)
  (switch-to-haskell))

(defun configure-haskell ()
  (turn-on-haskell-doc-mode)
  (haskell-indent-mode)
  (yas/minor-mode)
  (haskell-newline-binding)
  (haskell-complete-binding)
  (local-set-key (kbd "C-<f10>") 'switch-to-haskell)
  (local-set-key (kbd "C-<f5>") 'load-file-in-inf-haskell))

(defun configure-haskell-cabal ()
  (haskell-newline-binding))

(defun configure-haskell-inf ()
  (ghci-completion-mode)
  (haskell-complete-binding))

(add-hook 'haskell-mode-hook 'configure-haskell)
(add-hook 'inferior-haskell-mode-hook 'configure-haskell-inf)
(add-hook 'haskell-cabal-mode-hook 'configure-haskell-cabal)

(provide 'config-haskell)
