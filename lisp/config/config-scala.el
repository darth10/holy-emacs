;;; Configuration for Scala

(require 'config-common)

(defconfig configure-scala
  (require 'scala-mode2))

(add-hook 'scala-mode-hook 'configure-scala)

(provide 'config-scala)
