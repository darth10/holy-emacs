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

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (defmacro set-paredit-key (key function)
    `(define-key paredit-mode-map (kbd ,key) (quote ,function)))
  (defmacro unset-paredit-key (key)
    `(set-paredit-key ,key nil))
  (unset-paredit-key "M-s")
  (unset-paredit-key "ESC <up>")
  (unset-paredit-key "M-<up>")
  (set-paredit-key "ESC M-<up>" paredit-splice-sexp-killing-backward)
  (set-paredit-key "ESC ESC <up>" paredit-splice-sexp-killing-backward)
  (set-paredit-key "C-, C-, C-k" paredit-splice-sexp-killing-backward)
  (set-paredit-key "C-, , k" paredit-splice-sexp-killing-backward)
  (unset-paredit-key "ESC <down>")
  (unset-paredit-key "M-<down>")
  (set-paredit-key "ESC M-<down>" paredit-splice-sexp-killing-forward)
  (set-paredit-key "ESC ESC <down>" paredit-splice-sexp-killing-forward)
  (set-paredit-key "C-, C-k" paredit-splice-sexp-killing-forward)
  (set-paredit-key "C-, k" paredit-splice-sexp-killing-forward)
  (unset-paredit-key "C-<right>")
  (set-paredit-key "M-<right>" paredit-forward-slurp-sexp)
  (set-paredit-key "ESC <right>" paredit-forward-slurp-sexp)
  (set-paredit-key "C-, C-f" paredit-forward-slurp-sexp)
  (set-paredit-key "C-, f" paredit-forward-slurp-sexp)
  (unset-paredit-key "C-<left>")
  (set-paredit-key "M-<left>" paredit-forward-barf-sexp)
  (set-paredit-key "ESC <left>" paredit-forward-barf-sexp)
  (set-paredit-key "C-, C-b" paredit-forward-barf-sexp)
  (set-paredit-key "C-, b" paredit-forward-barf-sexp)
  (unset-paredit-key "ESC C-<right>")
  (set-paredit-key "ESC M-<right>" paredit-backward-barf-sexp)
  (set-paredit-key "ESC ESC <right>" paredit-backward-barf-sexp)
  (set-paredit-key "C-, C-, C-f" paredit-backward-barf-sexp)
  (set-paredit-key "C-, , f" paredit-backward-barf-sexp)
  (unset-paredit-key "ESC C-<left>")
  (set-paredit-key "ESC M-<left>" paredit-backward-slurp-sexp)
  (set-paredit-key "ESC ESC <left>" paredit-backward-slurp-sexp)
  (set-paredit-key "C-, C-, C-b" paredit-backward-slurp-sexp)
  (set-paredit-key "C-, , b" paredit-backward-slurp-sexp)
  (use-package lisp-mode
    :config
    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  (use-package clojure-mode :config (add-hook 'clojure-mode-hook 'paredit-mode))
  (use-package scheme :config (add-hook 'scheme-mode-hook 'paredit-mode)))

(provide 'config-lisps)
