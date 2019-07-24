;;; mod-lang-scala.el --- Configuration for Scala    -*- lexical-binding: t; -*-

(use-package scala-mode
  :ensure t
  :mode ("\\.scala\\'" . scala-mode))

(use-package sbt-mode
  :ensure t
  :mode ("\\.sbt\\'" . sbt-mode))

(use-package ensime
  :ensure t
  :after scala-mode)

(provide 'mod-lang-scala)
