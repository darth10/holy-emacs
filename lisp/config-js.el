;;; Configuration for JavaScript

(defun configure-js ()
  (local-set-key "\r" 'newline-and-indent))

(add-hook 'js-mode-hook 'configure-js)

(provide 'config-js)
