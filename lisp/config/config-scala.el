;;; Configuration for Scala

(use-package scala-mode
  :ensure t
  :mode ("\\.scala\\'" . scala-mode))

(use-package ensime
  :ensure t
  :defer 2)

(use-package sbt-mode
  :ensure t
  :defer 2)

(provide 'config-scala)
