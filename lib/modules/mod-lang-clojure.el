;;; mod-lang-clojure.el --- Configuration for Clojure   -*- lexical-binding: t; -*-

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . +clojure--highlight-sexp-setup))
  :init
  (defun +clojure--highlight-sexp-setup ()
    (+highlight-sexp:bind-keys 'clojure-mode-map)))

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . +clojure--clj-refactor-setup)
  :config
  (setq cljr-warn-on-eval nil)
  (defun +clojure--clj-refactor-setup ()
    (clj-refactor-mode t)
    (local-unset-key (kbd "C-:"))
    (cl-loop for key in holy-emacs-lang-apply-refactor-keys
             do (cljr-add-keybindings-with-prefix key))))

(use-package cider
  :after clojure-mode
  :hook (cider-repl-mode . paredit-mode)
  :lang (:map clojure-mode-map
         (:repl-start . cider-jack-in)
         (:repl-connect . cider-connect-clj)
         :map clojurescript-mode-map
         (:repl-start . cider-jack-in-cljs)
         (:repl-connect . cider-connect-cljs)
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
  :straight nil
  :after (clojure-mode cider)
  :hook ((cider-mode cider-repl-mode) . eldoc-mode))

(use-package cider-eval-sexp-fu
  :after (clojure-mode cider))

(provide 'mod-lang-clojure)
