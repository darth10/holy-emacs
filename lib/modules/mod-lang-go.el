;;; mod-lang-go.el --- Configuration for Go          -*- lexical-binding: t; -*-

;;; Requires a few Go packages:
;; $ go get -v github.com/mdempsky/gocode
;; $ go get -v github.com/rogpeppe/godef

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :lang (:map go-mode-map
         (:find-definition . godef-jump))
  :config
  (setq gofmt-show-errors 'echo)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'flycheck-mode))

(use-package company-go
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
         ("." . +go/company-go-complete)
         ("M-SPC" . company-go))
  :config
  (defun +go/company-go-complete ()
    (interactive)
    (insert ".")
    (call-interactively 'company-go)))

(provide 'mod-lang-go)
