;;; mod-lang-go.el --- Configuration for Go          -*- lexical-binding: t; -*-

;;; Requires a few Go packages:
;; $ go get -v github.com/mdempsky/gocode
;; $ go get -v github.com/rogpeppe/godef

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook ((before-save . gofmt-before-save)
         (go-mode . flycheck-mode))
  :lang (:map go-mode-map
         (:find-definition . godef-jump))
  :config
  (setq gofmt-show-errors 'echo))

(use-package company-go
  :after go-mode
  :lang (:comp (go-mode . company-go)))

(provide 'mod-lang-go)
