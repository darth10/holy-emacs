;;; mod-editor-navigation.el --- Editor packages for navigation  -*- lexical-binding: t; -*-

(use-package xref
  :straight nil
  :lang (:map global-map
         (:find-definition . xref-find-definitions))
  :bind ("C--" . pop-tag-mark))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package subword
  :straight nil
  :bind (("C-' c" . subword-mode)
         ("C-' C-c" . subword-mode)))

(use-package ibuffer
  :straight nil
  :bind ("C-x C-b" . ibuffer))

(use-package inflections)

(use-package smex
  :defer 2)

(use-package hideshow
  :straight nil
  :hook ((prog-mode conf-mode) . hs-minor-mode)
  :bind (:map hs-minor-mode-map
         ("C-c d" . hs-hide-block)
         ("C-c v d" . hs-hide-all)
         ("C-c s" . hs-show-block)
         ("C-c v s" . hs-show-all)))

(provide 'mod-editor-navigation)
