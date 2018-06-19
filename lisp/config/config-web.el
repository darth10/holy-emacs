;;; Configuration for Web

(use-package zencoding-mode
  :ensure t
  :defer 5
  :diminish zencoding-mode
  :bind (:map zencoding-mode-keymap
         ("M-SPC" . zencoding-expand-line)))

(use-package rainbow-mode
  :ensure t
  :defer 5
  :diminish rainbow-mode
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

  (custom-set-variables
   '(web-mode-enable-auto-quoting t)
   '(web-mode-enable-auto-expanding t))

  (use-package smartparens
    :config
    (defun configure-web-smartparens-mode ()
      (if smartparens-mode (smartparens-mode -1)))
    (add-hook 'web-mode-hook 'configure-web-smartparens-mode))

  (use-package zencoding-mode
    :config
    (defun configure-web-zencoding-mode ()
      (require 'zencoding-mode)
      (zencoding-mode t))
    (add-hook 'web-mode-hook 'configure-web-zencoding-mode)))

(provide 'config-web)
