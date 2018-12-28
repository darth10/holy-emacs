;;; Configuration for projectile

(use-package projectile
  :ensure t
  :defer 2
  :bind (:map projectile-mode-map
		 ("C-c p" . projectile-command-map))
  :config
  (projectile-mode t)
  (use-package helm-projectile
	:ensure t
	:config
	(setq projectile-completion-system 'helm)))

(provide 'config-projectile)
