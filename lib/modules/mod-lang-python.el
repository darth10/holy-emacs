;;; mod-lang-python.el --- Configuration for Python  -*- lexical-binding: t; -*-

(use-package python
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

(provide 'mod-lang-python)
