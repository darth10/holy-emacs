;;; core-themes.el --- holy-emacs themes             -*- lexical-binding: t; -*-

;; Note that `custom-safe-themes' must be set appropriately before setting
;; `custom-enabled-themes' to avoid load errors and user prompts.

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" ;; solarized-dark
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" ;; solarized-light
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" ;; spacemacs-dark
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" ;; spacemacs-light
     "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" ;; zenburn
     "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" ;; monokai
     "2f4f50d98073c01038b518066840638455657dc91dd1a225286d573926f36914" ;; heroku
     "f11e219c9d043cbd5f4b2e01713c2c24a948a98bed48828dc670bd64ae771aa1" ;; clues
     "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" ;; flatland
     default))))

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package spacemacs-theme
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package monokai-theme
  :ensure t
  :defer t)

(use-package heroku-theme
  :ensure t
  :defer t)

(use-package clues-theme
  :ensure t
  :defer t)

(use-package flatland-theme
  :ensure t
  :defer t)

(provide 'core-themes)
