;;; Configuration for Scala

(require 'scala-mode2)
(require 'config-common)

(defconfig configure-scala)
(add-hook 'scala-mode-hook 'configure-scala)

(provide 'config-scala)
