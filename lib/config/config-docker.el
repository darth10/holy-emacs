;;; Configuration for docker

(use-package docker
  :ensure t
  :defer 2
  :config
  (setq docker-container-shell-file-name "/bin/sh"))

(use-package dockerfile-mode
  :ensure t
  :defer 5)

(provide 'config-docker)
