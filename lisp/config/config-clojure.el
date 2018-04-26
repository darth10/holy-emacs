;;; Configuration for Clojure

(use-package subword
  :diminish subword-mode)

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (add-hook 'clojure-mode-hook 'subword-mode))

(use-package clojurescript-mode
  :ensure t
  :mode ("\\.cljs\\'" . clojurescript-mode))

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :config
  (setq cljr-warn-on-eval nil)

  (defun configure-clj-refactor ()
    (clj-refactor-mode t)
    (local-unset-key (kbd "C-:"))
    (cljr-add-keybindings-with-prefix "C-c ESC"))

  (add-hook 'clojure-mode-hook 'configure-clj-refactor))

(use-package cider
  :ensure t
  :bind (:map cider-mode-map
         ("C-x a a" . load-file-in-nrepl)
         ("C-x C-a C-a" . load-file-in-nrepl)
         ("C-<f5>" . load-file-in-nrepl)
         ("C-c C-k" . cider-load-buffer)
         ("C-c k" . cider-load-buffer)
         ("C-c C-l" . cider-load-file)
         ("C-c l" . cider-load-file)
         ("C-x T" . cider-test-run-ns-tests)
         ("C-x t" . cider-test-run-test)
         ("C-?" . cider-doc)
         :map cider-repl-mode-map
         ("C-x T" . cider-test-run-ns-tests)
         ("C-x t" . cider-test-run-test)
         ("C-?" . cider-doc)
         :map clojure-mode-map
         ("C-! C-o" . cider-connect)
         ("C-<f8>" . cider-connect)
         ("C-! C-r" . cider-jack-in)
         ("C-<f10>" . cider-jack-in)
         :map clojurescript-mode-map
         ("C-! C-r" . cider-jack-in-clojurescript)
         ("C-<f10>" . cider-jack-in-clojurescript))
  :config
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-inject-dependencies-at-jack-in nil)

  (defun load-file-in-nrepl ()
    (interactive)
    (cider-load-buffer)
    (cider-repl-set-ns (cider-current-ns))
    (cider-switch-to-repl-buffer))

  (use-package eldoc
    :config
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'eldoc-mode)))

(use-package cider-eval-sexp-fu
  :ensure t
  :config
  (defun config-init-cider-eval-sexp-fu ()
    (require 'cider-eval-sexp-fu))

  (add-hook 'cider-mode-hook 'config-init-cider-eval-sexp-fu))

(provide 'config-clojure)
