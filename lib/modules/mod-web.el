;;; mod-web.el --- Configuration for web development  -*- lexical-binding: t; -*-

(use-package emmet-mode
  :ensure t
  :defer 5
  :bind (:map emmet-mode-keymap
         ("M-SPC" . emmet-expand-line))
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package rainbow-mode
  :ensure t
  :defer 5
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))

(use-package web-mode
  :ensure t
  :defer 5
  :mode (("\\.html\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting t
        web-mode-enable-auto-expanding t)

  (use-package smartparens
    :config
    (defun configure-web-smartparens-mode ()
      (if smartparens-mode (smartparens-mode -1)))
    (add-hook 'web-mode-hook 'configure-web-smartparens-mode))

  (use-package emmet-mode
    :config
    (add-hook 'web-mode-hook 'emmet-mode)))

(use-package restclient
  :ensure t
  :defer 5)

(use-package handlebars-mode
  :ensure t)

(use-package web-beautify
  :ensure t)

(provide 'mod-web)
