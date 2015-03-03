;;; Configuration for Web (HTML, CSS)

(require 'config-common)
(require 'web-mode)

(defconfig configure-html
  (require 'rainbow-mode)
  (require 'zencoding-mode)
  (local-set-key (kbd "M-SPC") 'zencoding-expand-line))

(add-hook 'html-mode-hook 'configure-html)
(add-hook 'web-mode-hook 'configure-html)

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-enable-auto-quoting t)
(setq web-mode-enable-auto-expanding t)

(provide 'config-web)
