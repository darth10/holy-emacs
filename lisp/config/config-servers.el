;;; Configuration for servers

(use-package server
  :ensure t
  :defer 2
  :config
  (server-start))

(use-package edit-server
  :ensure t
  :if window-system
  :defer 2
  :config
  (edit-server-start))

(provide 'config-servers)
