;;; mod-editor-highlight.el --- Editor packages for syntax highlighting  -*- lexical-binding: t; -*-

(use-package hl-line
  :straight nil
  :bind (("C-' l" . hl-line-mode)
         ("C-' C-l" . hl-line-mode)
         ("C-<f4>" . hl-line-mode))
  :hook ((prog-mode org-mode dired-mode) . hl-line-mode))

(use-package highlight-sexp
  :commands (+highlight-sexp:bind-keys)
  :hook (highlight-sexp-mode . +highlight-sexp--set-hl-line)
  :config
  (defun +highlight-sexp--set-hl-line ()
    (hl-line-mode (if highlight-sexp-mode -1 t)))

  (defconst +highlight-sexp--keys
    '("C-<f12>"
      "C-' C-s"
      "C-' s"))

  (defun +highlight-sexp:bind-keys (mode-map)
    (core-bind-keys +highlight-sexp--keys #'highlight-sexp-mode mode-map)))

(use-package fic-mode
  :hook ((prog-mode org-mode dired-mode) . fic-mode)
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "HACK")))

(provide 'mod-editor-highlight)
