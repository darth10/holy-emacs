;;; mod-editor-help.el --- Editor packages for help  -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :bind (("C-' k" . which-key-mode)
         ("C-' C-k" . which-key-mode))
  :init
  (which-key-setup-side-window-bottom)
  (which-key-enable-god-mode-support)
  (setq which-key-max-description-length 24
        which-key-max-display-columns 4
        which-key-separator " : ")
  (unbind-key "C-h C-h")
  (which-key-mode t))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package woman
  :unless (core:is-windows-p)
  :bind ("C-x ?" . woman))

(provide 'mod-editor-help)
