;;; mod-lang-clojure.el --- Configuration for Clojure   -*- lexical-binding: t; -*-

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :config
  (defun +clojure--configure-clj-refactor ()
    (clj-refactor-mode t)
    (local-unset-key (kbd "C-:"))
    (cljr-add-keybindings-with-prefix "C-c '"))

  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook #'+clojure--configure-clj-refactor))

(use-package cider
  :ensure t
  :after clojure-mode
  :lang (:map clojure-mode-map
         (:repl-start . cider-jack-in)
         :map clojurescript-mode-map
         (:repl-start . cider-jack-in-cljs)
         :map cider-mode-map
         (:find-definition . +clojure/find-definition)
         (:eval-buffer . +clojure/eval-buffer-and-switch-ns)
         (:load-file . cider-load-file)
         (:test-file . cider-test-run-ns-tests)
         (:test-all . cider-test-run-project-tests))
  :config
  (defun +clojure/eval-buffer-and-switch-ns ()
    (interactive)
    (cider-load-buffer)
    (cider-repl-set-ns (cider-current-ns))
    (cider-switch-to-repl-buffer))

  (defun +clojure/find-definition ()
    (interactive)
    (cider-find-var (symbol-at-point)))

  (setq cider-auto-select-error-buffer t
        cider-repl-popup-stacktraces t
        cider-inject-dependencies-at-jack-in nil))

(use-package eldoc
  :after (clojure-mode cider)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package cider-eval-sexp-fu
  :ensure t
  :after (clojure-mode cider)
  :config
  (defun +clojure/config-init-cider-eval-sexp-fu ()
    (require 'cider-eval-sexp-fu))

  (add-hook 'cider-mode-hook #'+clojure/config-init-cider-eval-sexp-fu))

(provide 'mod-lang-clojure)
