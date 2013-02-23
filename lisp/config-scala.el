;;; Configuration for Scala

(add-to-list 'load-path "~/.emacs.d/scala-emacs")
(require 'scala-mode-auto)

(defun configure-scala ()
  (scala-mode-feature-electric-mode))

(add-hook 'scala-mode-hook 'config-scala)

(provide 'config-scala)
