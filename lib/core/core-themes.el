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
     "e4486d0ad184fb7511e391b6ecb8c4d7e5ab29e2d33bc65403e2315dbacaa4aa" ;; zenburn
     "837f2d1e6038d05f29bbcc0dc39dbbc51e5c9a079e8ecd3b6ef09fc0b149ceb1" ;; monokai
     "60c23c3a831c9f28b10084e8642b7d987d33be3faee8f68d68d1cf6b171041da" ;; heroku
     "ed91d4e59412defda16b551eb705213773531f30eb95b69319ecd142fab118ca" ;; clues
     "a56cc18045d90be8f770ae409fc86274f8e5de2999a16b604ff84f8015e8d1e5" ;; flatland
     default))))

(use-package solarized-theme
  :defer t)

(use-package spacemacs-theme
  :defer t)

(use-package zenburn-theme
  :defer t)

(use-package monokai-theme
  :defer t)

(use-package heroku-theme
  :defer t)

(use-package clues-theme
  :defer t)

(use-package flatland-theme
  :defer t)

(provide 'core-themes)
