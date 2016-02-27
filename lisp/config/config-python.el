;;; Configuration for Python

(require 'config-common)


(defun run-python-debugger ()
  (interactive)
  (let* ((debug-command (concat  "pdb " buffer-file-name))
         (user-debug-command (read-string "Run pdb (like this): "
                                          debug-command)))
    (pdb user-debug-command)))

(defun load-file-in-inf-python ()
  (interactive)
  (python-shell-send-file (buffer-file-name)))

(defconfig configure-py
  ;; (smartparens-mode)
  (local-set-key (kbd "C-<f5>") 'load-file-in-inf-python)
  (local-set-key (kbd "C-x C-a C-a") 'load-file-in-inf-python)
  (local-set-key (kbd "C-x a a") 'load-file-in-inf-python)
  (local-set-key (kbd "C-<f10>") 'run-python)
  (local-set-key (kbd "C-! C-r") 'run-python)
  (local-set-key (kbd "C-<f11>") 'run-python-debugger)
  (local-set-key (kbd "C-! C-d") 'run-python-debugger))

(add-hook 'python-mode-hook 'configure-py)

(provide 'config-python)
