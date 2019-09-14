;;; mod-lang-scala.el --- Configuration for Scala    -*- lexical-binding: t; -*-

(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode))

(use-package sbt-mode
  :mode ("\\.sbt\\'" . sbt-mode))

(use-package ensime
  :straight (:host github
             :repo "ensime/ensime-emacs"
             :branch "2.0")
  :after scala-mode)

(provide 'mod-lang-scala)
