;;; Configuration for JavaScript

(require 'config-common)

(defconfig configure-js
  (local-set-key (kbd "C-<f10>") 'run-js)
  (local-set-key (kbd "C-<f5>") 'js-send-buffer-and-go))

(defun configure-js-inf ()
  (ansi-color-for-comint-mode-on)
  (local-set-key (kbd "TAB") 'dabbrev-expand)
  (add-to-list 'comint-preoutput-filter-functions   ;; Deal with some prompt nonsense
	       (lambda (output)
		 (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output))))

(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook 'configure-js-inf)

(add-hook 'js-mode-hook 'configure-js)

(provide 'config-js)
