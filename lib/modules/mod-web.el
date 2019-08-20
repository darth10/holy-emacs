;;; mod-web.el --- Configuration for web development  -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
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
        web-mode-enable-auto-expanding t))

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :after (:any sgml-mode web-mode css-mode)
  :bind (:map emmet-mode-keymap
         ("M-SPC" . emmet-expand-line))
  :hook ((sgml-mode web-mode css-mode) . emmet-mode))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :after (:any web-mode css-mode)
  :hook ((web-mode css-mode) . rainbow-mode))

(use-package restclient
  :ensure t
  :commands (restclient-mode))

(use-package handlebars-mode
  :ensure t)

(use-package web-beautify
  :ensure t)

(provide 'mod-web)
