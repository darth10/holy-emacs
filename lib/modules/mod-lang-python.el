;;; Configuration for Python

(use-package python
  ;; :ensure t
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
         ("C-<f5>" . load-file-in-inf-python)
         ("C-x C-a C-a" . load-file-in-inf-python)
         ("C-x a a" . load-file-in-inf-python)
         ("C-<f10>" . run-python)
         ("C-! C-r" . run-python)
         ("C-<f11>" . run-python-debugger)
         ("C-! C-d" . run-python-debugger))
  :config
  (defun run-python-debugger ()
    (interactive)
    (let* ((debug-command (concat  "pdb " buffer-file-name))
           (user-debug-command (read-string "Run pdb (like this): "
                                            debug-command)))
      (pdb user-debug-command)))

  (defun load-file-in-inf-python ()
    (interactive)
    (python-shell-send-file (buffer-file-name))))

(provide 'mod-lang-python)
