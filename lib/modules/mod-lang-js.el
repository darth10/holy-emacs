;;; Configuration for JavaScript

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2)

  (use-package js2-refactor
    :ensure t
    :config
    (add-hook 'js2-mode-hook 'js2-refactor-mode))

  (use-package tern
    :ensure t
    :config
    (add-hook 'js2-mode-hook 'tern-mode)

    (use-package company-tern
      :ensure t
      :config
      (bind-key "M-SPC" 'company-tern js2-mode-map)
      (bind-key "M-SPC" 'company-tern js2-jsx-mode-map))))

(use-package web-beautify
  :ensure t)

(provide 'mod-lang-js)
