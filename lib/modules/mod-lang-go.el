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
  (setq gofmt-show-errors 'echo)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (use-package company-go
    :ensure t
    :bind (:map go-mode-map
           ("." . company-go-complete)
           ("M-SPC" . company-go))
    :config
    (defun company-go-complete ()
      (interactive)
      (insert ".")
      (call-interactively 'company-go))))

(provide 'mod-lang-go)
