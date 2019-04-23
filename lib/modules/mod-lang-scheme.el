;;; Configuration for Scheme

(use-package geiser
  :ensure t
  :bind (:map scheme-mode-map
         ("C-<f10>" . run-geiser)
         ("C-! C-r" . run-geiser)
         ("C-<f5>" . scheme-load-and-repl)
         ("C-x C-a C-a" . scheme-load-and-repl)
         ("C-x a a" . scheme-load-and-repl)
         ("C-c C-l" . scheme-load-current-file)
         ("C-c l" . scheme-load-current-file))

  :config

  (defun scheme-load-and-repl ()
    (interactive)
    (geiser-compile-current-buffer)
    (geiser-mode-switch-to-repl t))

  (defun scheme-load-current-file ()
    (interactive)
    (geiser-load-file buffer-file-name)))

(provide 'mod-lang-scheme)
