;;; Configuration for hideshow

(use-package hideshow
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-c d" . hs-hide-block)
              ("C-c a d" . hs-hide-all)
              ("C-c C-d" . hs-hide-block)
              ("C-c C-a C-d" . hs-hide-all)
              ("C-c s" . hs-show-block)
              ("C-c a s" . hs-show-all)
              ("C-c C-s" . hs-show-block)
              ("C-c C-a C-s" . hs-show-all))
  :config
  (add-hook 'clojure-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'csharp-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook 'hs-minor-mode)
  (add-hook 'lisp-mode-hook 'hs-minor-mode)
  (add-hook 'sh-mode-hook 'hs-minor-mode))

(provide 'config-hideshow)
