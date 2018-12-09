;;; Configuration for Go

;;; Requires a few Go packages:
;; $ go get -v github.com/mdempsky/gocode
;; $ go get -v github.com/rogpeppe/godef

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ("M-." . godef-jump))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (use-package company-go
    :ensure t
    :bind (:map go-mode-map
           ("." . company-go-intellisense)
           ("M-SPC" . company-go))
    :config
    (defun company-go-intellisense ()
      (interactive)
      (insert ".")
      (call-interactively 'company-go))))


(provide 'config-go)
