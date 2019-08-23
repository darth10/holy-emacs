;;; core-themes.el --- holy-emacs themes             -*- lexical-binding: t; -*-

;; Note that `custom-safe-themes' must be set appropriately before setting
;; `custom-enabled-themes' to avoid load errors and user prompts.

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" ;; solarized-dark
     "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" ;; solarized-light
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" ;; spacemacs-dark
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" ;; spacemacs-light
     "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" ;; zenburn
     "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" ;; monokai
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
