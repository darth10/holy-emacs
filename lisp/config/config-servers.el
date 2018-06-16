;;; Configuration for servers

(use-package server
  :ensure t
  :defer 4
  :config
  (server-start))

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(provide 'config-servers)
