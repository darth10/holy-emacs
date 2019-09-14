;;; mod-web.el --- Configuration for web development  -*- lexical-binding: t; -*-

(use-package web-mode
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
  :commands (emmet-mode)
  :hook ((sgml-mode web-mode css-mode) . emmet-mode)
  :after (:any sgml-mode web-mode css-mode)
  :bind (:map emmet-mode-keymap
         ("M-SPC" . emmet-expand-line)))

(use-package rainbow-mode
  :after (:any web-mode css-mode)
  :commands (rainbow-mode)
  :hook ((web-mode css-mode) . rainbow-mode))

(use-package restclient
  :commands (restclient-mode))

(use-package handlebars-mode)

(use-package web-beautify)

(provide 'mod-web)
