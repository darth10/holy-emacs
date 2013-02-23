;;; Configuration for Haskell

(require 'haskell-mode)
(require 'ghci-completion)

(defun haskell-bindings ()
  (local-set-key (kbd "C-<tab>") 'dabbrev-expand))

(defun load-file-in-inf-haskell ()
  (interactive)
  (inferior-haskell-load-file)
  (switch-to-haskell))

(defun configure-haskell ()
  (turn-on-haskell-doc-mode)
  (haskell-indent-mode)
  (yas/minor-mode)
  (haskell-bindings)
  (local-set-key (kbd "C-x <f10>") 'switch-to-haskell)
  (local-set-key (kbd "C-x <f5>") 'load-file-in-inf-haskell))

(defun configure-haskell-inf ()
  (ghci-completion-mode)
  (haskell-bindings))

(add-hook 'haskell-mode-hook 'configure-haskell)
(add-hook 'inferior-haskell-mode-hook 'configure-haskell-inf)

(provide 'config-haskell)
