;;; Configuration for Python

(require 'config-common)


(defun run-python-debugger ()
  (interactive)
  (let* ((debug-command (concat  "pdb " buffer-file-name))
         (user-debug-command (read-string "Run pdb (like this): "
                                          debug-command)))
    (pdb user-debug-command)))

(defconfig configure-py
  (local-set-key (kbd "C-<f10>") 'run-python)
  (local-set-key (kbd "C-<f11>") 'run-python-debugger))

(add-hook 'python-mode-hook 'configure-py)

(provide 'config-python)
