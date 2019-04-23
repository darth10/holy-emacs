;;; Configuration for projectile

(use-package projectile
  :ensure t
  :defer 2
  :bind (:map projectile-mode-map
		 ("C-c p" . projectile-command-map))
  :init
  (setq projectile-cache-file (concat core-var-cache-dir-full-path "projectile/cache")
        projectile-known-projects-file (concat core-var-cache-dir-full-path "projectile/bookmarks.eld"))
  :config
  (projectile-mode t)
  (use-package helm-projectile
	:ensure t
	:config
	(setq projectile-completion-system 'helm)))

(provide 'mod-projectile)
