;;; mod-lang-python.el --- Configuration for Python  -*- lexical-binding: t; -*-

;;; Requires jedi server. Install it using M-x jedi:install-server.

(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :lang (:map python-mode-map
         (:repl-start . run-python)
         (:eval-buffer . +python/load-file)
         (:load-file . +python/load-file)
         (:debugger . +python/run-debugger))
  :config
  (defun +python/run-debugger ()
    (interactive)
    (let* ((debug-command (concat  "pdb " buffer-file-name))
           (user-debug-command (read-string "Run pdb (like this): "
                                            debug-command)))
      (pdb user-debug-command)))

  (defun +python/load-file ()
    (interactive)
    (python-shell-send-file (buffer-file-name))))

(use-package python-environment
  :defer 2
  :init
  (setq python-environment-directory
        (concat core-var-cache-dir-full-path "python-environments/")))

(use-package jedi-core
  :defer 2
  :lang (:map python-mode-map
         (:find-definition . jedi:goto-definition)))

(use-package company-jedi
  :defer 2
  :lang (:comp (python-mode . company-jedi)))

(provide 'mod-lang-python)
