;;; Configuration for servers

(use-package server
  :ensure t
  :config
  (server-start))

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(provide 'config-servers)
