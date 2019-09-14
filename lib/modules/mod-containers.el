;;; mod-containers.el --- Packages for Docker containers  -*- lexical-binding: t; -*-

(use-package docker
  :commands (docker)
  :config
  (setq docker-container-shell-file-name "/bin/sh"))

(use-package dockerfile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

(use-package kubernetes
  :commands (kubernetes-overview)
  :hook (kubernetes-mode . +kubernetes--popup-setup)
  :bind (:map kubernetes-mode-map
         ("N" . kubernetes-set-namespace))
  :config
  (defun +kubernetes--popup-setup ()
    (magit-define-popup-action 'kubernetes-overview-popup
      ?N "Set namespace" 'kubernetes-set-namespace ?c)))

(use-package k8s-mode
  :commands (k8s-mode)
  :config
  (setq k8s-search-documentation-browser-function 'browse-url))

(provide 'mod-containers)
