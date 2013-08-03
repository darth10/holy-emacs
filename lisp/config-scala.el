;;; Configuration for Scala

(add-to-list 'load-path "~/.emacs.d/lisp.git/scala-mode2")
(require 'scala-mode2)

(defun configure-scala ()
  (local-set-key "\r" 'newline-and-indent))

(add-hook 'scala-mode-hook 'configure-scala)

(provide 'config-scala)
