;;; mod-lang-js.el --- Configuration for JavaScript  -*- lexical-binding: t; -*-

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2))

(use-package js2-refactor
  :ensure t
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package tern
  :ensure t
  :after js2-mode
  :lang (:map js2-mode-map
         (:find-definition . tern-find-definition))
  :config
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :ensure t
  :after tern
  :lang (:comp (js2-mode . company-tern)))

(provide 'mod-lang-js)
