;;; mod-editor-navigation.el --- Editor packages for navigation  -*- lexical-binding: t; -*-

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package subword
  :bind (("C-' c" . subword-mode)
         ("C-' C-c" . subword-mode)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package inflections
  :ensure t)

(use-package smex
  :ensure t
  :defer 2)

(use-package hideshow
  :bind (:map hs-minor-mode-map
         ("C-c d" . hs-hide-block)
         ("C-c v d" . hs-hide-all)
         ("C-c s" . hs-show-block)
         ("C-c v s" . hs-show-all))
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(provide 'mod-editor-navigation)
