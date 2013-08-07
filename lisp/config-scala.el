;;; Configuration for Scala

(add-to-list 'load-path "~/.emacs.d/lisp.git/scala-mode2")

(require 'scala-mode2)
(require 'config-common)

(defconfig configure-scala)
(add-hook 'scala-mode-hook 'configure-scala)

(provide 'config-scala)
