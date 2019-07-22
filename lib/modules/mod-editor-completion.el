;;; mod-editor-completion.el --- Editor packages for code completion  -*- lexical-binding: t; -*-

(use-package abbrev
  :defer 2)

(use-package company
  :ensure t
  :defer 2
  :bind (("C-' a" . global-company-mode)
         ("C-' C-a" . global-company-mode)
         ("M-SPC" . company-manual-begin))
  :config
  (global-company-mode t))

(use-package yasnippet
  :ensure t
  :defer 2
  :bind (("C-' C-y" . yas-global-mode)
         ("C-' y" . yas-global-mode))
  :config
  (custom-set-faces
   '(yas-field-highlight-face ((t (:inherit 'region)))))

  (let* ((temp-yas-snippet-dirs
          (append yas-snippet-dirs
                  (list (expand-file-name (concat core-var-dir-path "snippets")
                                          user-emacs-directory))))
         (temp-yas-snippet-dirs
          (delete yas--default-user-snippets-dir temp-yas-snippet-dirs)))
    (setq yas-snippet-dirs temp-yas-snippet-dirs))

  (use-package yasnippet-snippets
    :ensure t)

  (yas-global-mode t))

(provide 'mod-editor-completion)
