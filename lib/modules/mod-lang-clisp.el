;;; mod-lang-clisp.el --- Configuration for Common Lisp  -*- lexical-binding: t; -*-

(use-package lisp-mode
  :straight nil
  :hook ((lisp-mode . paredit-mode)
         (lisp-mode . +clisp--highlight-sexp-setup))
  :init
  (defun +clisp--highlight-sexp-setup ()
    (+highlight-sexp:bind-keys 'lisp-mode-map)))

(use-package slime
  :commands (slime slime-connnect)
  :hook (slime-repl-mode . paredit-mode)
  :lang (:map lisp-mode-map
         (:repl-start . slime)
         (:repl-connect . slime-connect)
         :map slime-mode-map
         (:find-definition . slime-edit-definition)
         (:eval-buffer . slime-eval-buffer)
         (:load-file . slime-load-file)
         (:compile-file . slime-compile-file))
  :config
  (custom-set-variables
   '(inferior-lisp-program "sbcl")
   '(slime-lisp-implementations (sbcl ("sbcl")))
   '(slime-default-lisp 'sbcl))
  (slime-setup '(slime-fancy slime-asdf)))

(use-package slime-company
  :after slime
  :lang (:comp (slime-mode . company-slime)
         :comp (slime-repl-mode . company-slime)))

(provide 'mod-lang-clisp)
