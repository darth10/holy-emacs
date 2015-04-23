;;; Configuration for Helm

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-git-files)
(require 'helm-swoop)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(global-set-key (kbd "C-<f2>") 'helm-imenu)
(global-set-key (kbd "C-c ;") 'helm-imenu)
(global-set-key (kbd "C-c C-;") 'helm-imenu)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-~") 'helm-mark-ring)
(global-set-key (kbd "M-]") 'helm-swoop)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(helm-mode)

(provide 'config-helm)
