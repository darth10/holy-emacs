;;; Configuration for JavaScript

(defun configure-js ()
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key (kbd "C-<f10>") 'run-js)
  (local-set-key (kbd "C-<f5>") 'js-send-buffer-and-go))

(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
	(ansi-color-for-comint-mode-on)
	;; Deal with some prompt nonsense
	(add-to-list
	 'comint-preoutput-filter-functions
	 (lambda (output)
	   (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

(add-hook 'js-mode-hook 'configure-js)

(provide 'config-js)
