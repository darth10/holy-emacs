;;; Configuration for Scala

(add-to-list 'load-path "~/.emacs.d/scala-emacs")
(require 'scala-mode-auto)

(defun configure-scala ()
  (scala-mode-feature-electric-mode)
  (local-set-key "\r" 'newline-and-indent))

(add-hook 'scala-mode-hook 'configure-scala)

(provide 'config-scala)
