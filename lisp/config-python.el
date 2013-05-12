;;; Configuration for Python

(defun configure-py ()
  (local-set-key "\r" 'newline-and-indent))

(add-hook 'python-mode-hook 'configure-py)

(provide 'config-python)
