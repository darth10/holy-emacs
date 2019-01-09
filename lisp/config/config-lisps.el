;;; Configuration for Lisps

(use-package highlight-sexp
  :ensure t
  :config

  (defun hl-sexp-set-hl-line ()
    (interactive)
    (hl-line-mode (if highlight-sexp-mode -1 t)))
  (add-hook 'highlight-sexp-mode-hook 'hl-sexp-set-hl-line)

  (defconst highlight-sexp-keys
	'("C-<f12>"
	  "C-' C-s"
	  "C-' s"))

  (defun set-highlight-sexp-keys (mode-map)
    (cl-loop for key in highlight-sexp-keys
             collect key
             and do (bind-key (kbd key) 'highlight-sexp-mode mode-map)))

  (use-package lisp-mode
    :config
	(set-highlight-sexp-keys lisp-mode-map)
	(set-highlight-sexp-keys emacs-lisp-mode-map))
  (use-package clojure-mode :config (set-highlight-sexp-keys clojure-mode-map))
  (use-package scheme :config (set-highlight-sexp-keys scheme-mode-map)))

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
  :defer 2
  :config
  (custom-set-faces
   '(eval-sexp-fu-flash ((t (:background "green" :foreground "black")))))

  (defun config-init-eval-sexp-fu ()
    (require 'eval-sexp-fu))

  (add-hook 'lisp-mode-hook 'config-init-eval-sexp-fu)
  (add-hook 'emacs-lisp-mode-hook 'config-init-eval-sexp-fu)
  (add-hook 'eshell-mode-hook 'config-init-eval-sexp-fu))

(provide 'config-lisps)
