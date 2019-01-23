;;; Configuration for Lisps

(use-package highlight-sexp
  :ensure t
  :defer 2
  :config

  (defun +highlight-sexp-set-hl-line ()
    (interactive)
    (hl-line-mode (if highlight-sexp-mode -1 t)))
  (add-hook 'highlight-sexp-mode-hook #'+highlight-sexp-set-hl-line)

  (defconst +highlight-sexp-keys
    '("C-<f12>"
      "C-' C-s"
      "C-' s"))

  (defun +highlight-sexp-bind-keys (mode-map)
    (core-bind-keys +highlight-sexp-keys #'highlight-sexp-mode mode-map))

  (use-package lisp-mode
    :config
    (+highlight-sexp-bind-keys 'lisp-mode-map)
    (+highlight-sexp-bind-keys 'emacs-lisp-mode-map))
  (use-package clojure-mode :config (+highlight-sexp-bind-keys 'clojure-mode-map))
  (use-package scheme :config (+highlight-sexp-bind-keys 'scheme-mode-map)))

(use-package paredit
  :ensure t
  :defer 2
  :diminish paredit-mode
  :bind (("C-' (" . paredit-mode)
         ("C-' C-(" . paredit-mode))
  :config
  (defun +paredit-bind-key (key function)
    (bind-key key function paredit-mode-map))
  (defun +paredit-unbind-key (key)
    (unbind-key key paredit-mode-map))

  (+paredit-unbind-key "M-s")
  (+paredit-unbind-key "ESC <up>")
  (+paredit-unbind-key "M-<up>")
  (+paredit-bind-key "ESC M-<up>" 'paredit-splice-sexp-killing-backward)
  (+paredit-bind-key "ESC ESC <up>" 'paredit-splice-sexp-killing-backward)
  (+paredit-bind-key "C-, C-, C-k" 'paredit-splice-sexp-killing-backward)
  (+paredit-bind-key "C-, , k" 'paredit-splice-sexp-killing-backward)
  (+paredit-unbind-key "ESC <down>")
  (+paredit-unbind-key "M-<down>")
  (+paredit-bind-key "ESC M-<down>" 'paredit-splice-sexp-killing-forward)
  (+paredit-bind-key "ESC ESC <down>" 'paredit-splice-sexp-killing-forward)
  (+paredit-bind-key "C-, C-k" 'paredit-splice-sexp-killing-forward)
  (+paredit-bind-key "C-, k" 'paredit-splice-sexp-killing-forward)
  (+paredit-unbind-key "C-<right>")
  (+paredit-bind-key "M-<right>" 'paredit-forward-slurp-sexp)
  (+paredit-bind-key "ESC <right>" 'paredit-forward-slurp-sexp)
  (+paredit-bind-key "C-, C-f" 'paredit-forward-slurp-sexp)
  (+paredit-bind-key "C-, f" 'paredit-forward-slurp-sexp)
  (+paredit-unbind-key "C-<left>")
  (+paredit-bind-key "M-<left>" 'paredit-forward-barf-sexp)
  (+paredit-bind-key "ESC <left>" 'paredit-forward-barf-sexp)
  (+paredit-bind-key "C-, C-b" 'paredit-forward-barf-sexp)
  (+paredit-bind-key "C-, b" 'paredit-forward-barf-sexp)
  (+paredit-unbind-key "ESC C-<right>")
  (+paredit-bind-key "ESC M-<right>" 'paredit-backward-barf-sexp)
  (+paredit-bind-key "ESC ESC <right>" 'paredit-backward-barf-sexp)
  (+paredit-bind-key "C-, C-, C-f" 'paredit-backward-barf-sexp)
  (+paredit-bind-key "C-, , f" 'paredit-backward-barf-sexp)
  (+paredit-unbind-key "ESC C-<left>")
  (+paredit-bind-key "ESC M-<left>" 'paredit-backward-slurp-sexp)
  (+paredit-bind-key "ESC ESC <left>" 'paredit-backward-slurp-sexp)
  (+paredit-bind-key "C-, C-, C-b" 'paredit-backward-slurp-sexp)
  (+paredit-bind-key "C-, , b" 'paredit-backward-slurp-sexp)

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
  :defer 2
  :config
  (custom-set-faces
   '(eval-sexp-fu-flash ((t (:background "green" :foreground "black")))))

  (defun +eval-sexp-fu-init ()
    (require 'eval-sexp-fu))

  (add-hook 'lisp-mode-hook '+eval-sexp-fu-init)
  (add-hook 'emacs-lisp-mode-hook '+eval-sexp-fu-init)
  (add-hook 'eshell-mode-hook '+eval-sexp-fu-init))

(provide 'config-lisps)
