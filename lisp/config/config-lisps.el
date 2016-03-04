;;; Configuration for Lisps

(use-package highlight-sexps
  :load-path "lisp/lib/"
  :diminish highlight-sexps-mode
  :bind (:map clojure-mode-map
              ("C-<f12>" . highlight-sexps-mode)
              ("C-' s" . highlight-sexps-mode)
              ("C-' C-s" . highlight-sexps-mode)
              :map emacs-lisp-mode-map
              ("C-<f12>" . highlight-sexps-mode)
              ("C-' s" . highlight-sexps-mode)
              ("C-' C-s" . highlight-sexps-mode)
              :map lisp-mode-map
              ("C-<f12>" . highlight-sexps-mode)
              ("C-' s" . highlight-sexps-mode)
              ("C-' C-s" . highlight-sexps-mode))
  :config
  (defun hl-sexp-set-hl-line ()
    (interactive)
    (hl-line-mode (if highlight-sexps-mode -1 t)))
  (add-hook 'highlight-sexps-mode-hook 'hl-sexp-set-hl-line)
  (use-package scheme
      :bind (:map scheme-mode-map
              ("C-<f12>" . highlight-sexps-mode)
              ("C-' s" . highlight-sexps-mode)
              ("C-' C-s" . highlight-sexps-mode))))

(provide 'config-lisps)
