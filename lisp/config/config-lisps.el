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
  :bind (("C-' (" . paredit-mode)
         ("C-' C-(" . paredit-mode))
  :config
  (defmacro set-paredit-key (key function)
    `(bind-key (kbd ,key) (quote ,function) paredit-mode-map))
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

  (add-hook 'eshell-mode-hook 'paredit-mode)

  (use-package smartparens
    :config
    (defun disable-smartparens-mode ()
      (interactive)
      (smartparens-mode (if paredit-mode -1 t)))
    (add-hook 'paredit-mode-hook 'disable-smartparens-mode))
  (use-package lisp-mode
    :config
    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  (use-package ielm
    :config
    (add-hook 'ielm-mode-hook 'paredit-mode))
  (use-package clojure-mode :config (add-hook 'clojure-mode-hook 'paredit-mode))
  (use-package cider :config (add-hook 'cider-repl-mode-hook 'paredit-mode))
  (use-package scheme :config (add-hook 'scheme-mode-hook 'paredit-mode))
  (use-package geiser :config (add-hook 'geiser-repl-mode-hook 'paredit-mode)))

(use-package eval-sexp-fu
  :ensure t
  :config
  (custom-set-faces
   '(eval-sexp-fu-flash ((t (:background "green" :foreground "black")))))

  (defun config-init-eval-sexp-fu ()
    (require 'eval-sexp-fu))

  (add-hook 'lisp-mode-hook 'config-init-eval-sexp-fu)
  (add-hook 'emacs-lisp-mode-hook 'config-init-eval-sexp-fu)
  (add-hook 'eshell-mode-hook 'config-init-eval-sexp-fu))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eshell-mode-hook 'eldoc-mode))

;; (use-package prog-mode
;;   :defer 4
;;   :bind (("C-' p" . prettify-symbols-mode)
;;          ("C-' C-p" . prettify-symbols-mode))
;;   :config
;;   (use-package lisp-mode
;;     :config
;;     (add-hook 'lisp-mode-hook 'prettify-symbols-mode)
;;     (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))
;;   (use-package ielm
;;     :config
;;     (add-hook 'ielm-mode-hook 'prettify-symbols-mode))
;;   (use-package clojure-mode
;;     :config
;;     (add-to-list 'prettify-symbols-alist '("fn" . 955))
;;     (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
;;     (add-hook 'clojurescript-mode-hook 'prettify-symbols-mode))
;;   (use-package cider :config (add-hook 'cider-repl-mode-hook 'prettify-symbols-mode))
;;   (use-package scheme
;;     :config
;;     (add-hook 'scheme-mode-hook 'prettify-symbols-mode)
;;     (add-hook 'scheme-mode-hook
;;               (lambda ()
;;                 (add-to-list 'prettify-symbols-alist '("lambda" . 955)))))
;;   (use-package geiser :config (add-hook 'geiser-repl-mode-hook 'prettify-symbols-mode)))

(provide 'config-lisps)
