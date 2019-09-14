;;; mod-editor-completion.el --- Editor packages for code completion  -*- lexical-binding: t; -*-

(use-package abbrev
  :straight nil
  :defer 2)

(use-package company
  :bind (("C-' a" . global-company-mode)
         ("C-' C-a" . global-company-mode)
         ("M-SPC" . company-manual-begin))
  :config
  (global-company-mode t))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (("C-' C-y" . yas-minor-mode)
         ("C-' y" . yas-minor-mode)
         :map yas-minor-mode-map
         ("C-c & s" . yas-insert-snippet))
  :custom-face
  (yas-field-highlight-face ((t (:inherit 'region))))
  :config
  (let* ((temp-yas-snippet-dirs
          (append yas-snippet-dirs
                  (list (expand-file-name (concat core-var-dir-path "snippets")
                                          user-emacs-directory))))
         (temp-yas-snippet-dirs
          (delete yas--default-user-snippets-dir temp-yas-snippet-dirs)))
    (setq yas-snippet-dirs temp-yas-snippet-dirs))

  (yas-reload-all))

(use-package yasnippet-snippets
  :after yas-minor-mode)

(provide 'mod-editor-completion)
