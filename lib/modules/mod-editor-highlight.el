;;; mod-editor-highlight.el --- Editor packages for syntax highlighting  -*- lexical-binding: t; -*-

(use-package hl-line
  :bind (("C-' l" . hl-line-mode)
         ("C-' C-l" . hl-line-mode)
         ("C-<f4>" . hl-line-mode))
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'org-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(use-package highlight-sexp
  :quelpa (highlight-sexp :fetcher github :repo "daimrod/highlight-sexp")
  :defer 2
  :config
  (defun +highlight-sexp--set-hl-line ()
    (hl-line-mode (if highlight-sexp-mode -1 t)))

  (add-hook 'highlight-sexp-mode-hook #'+highlight-sexp--set-hl-line)

  (defconst +highlight-sexp--keys
    '("C-<f12>"
      "C-' C-s"
      "C-' s"))

  (defun +highlight-sexp--bind-keys (mode-map)
    (core-bind-keys +highlight-sexp--keys #'highlight-sexp-mode mode-map))

  (use-package lisp-mode
    :config
    (+highlight-sexp--bind-keys 'lisp-mode-map)
    (+highlight-sexp--bind-keys 'emacs-lisp-mode-map))
  (use-package clojure-mode :config (+highlight-sexp--bind-keys 'clojure-mode-map))
  (use-package scheme :config (+highlight-sexp--bind-keys 'scheme-mode-map)))

(use-package fic-mode
  :ensure t
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "HACK"))
  (add-hook 'prog-mode-hook #'fic-mode)
  (add-hook 'conf-mode-hook #'fic-mode))

(provide 'mod-editor-highlight)
