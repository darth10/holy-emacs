;;; Configuration for JavaScript

(require 'config-common)

(defun slime-connect-to-repl ()
  (interactive)
  (slime-connect "127.0.0.1" 4005))

(defconfig configure-js
  (require 'slime)
  (require 'tern)
  (require 'slime-js)
  (js2-minor-mode t)
  (tern-mode t)
  (tern-ac-setup)
  (slime-js-minor-mode 1)
  (local-set-key (kbd "M-SPC") 'tern-ac-complete)
  (local-unset-key (kbd "C-x C-e"))
  (local-set-key (kbd "C-x C-e") 'slime-js-send-defun)
  (local-set-key (kbd "C-<f10>") 'slime-connect-to-repl)
  (local-set-key (kbd "C-<f8>") 'slime-connect)
  ;; FIXME should be slime-js-reload
  (local-set-key (kbd "C-<f5>") 'slime-eval-buffer))

(defun slime-ac-key ()
  (interactive)
  (insert ".")
  (ac-complete-slime))

(defun configure-slime ()
  (interactive)
  (require 'slime)
  (require 'ac-slime)
  (set-up-slime-ac 1)
  (local-set-key (kbd ".") 'slime-ac-key)
  (local-set-key (kbd "M-SPC") 'ac-complete-slime))

(add-hook 'js-mode-hook 'configure-js)
(add-hook 'slime-repl-mode-hook 'configure-slime)

(setq js2-basic-offset 2)

(provide 'config-js)
