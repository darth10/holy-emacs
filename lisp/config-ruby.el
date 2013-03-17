;;; Configuration for Ruby

(require 'ruby-mode)

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(defun rinari-run-all-test ()
  (interactive)
  (rinari-rake "test"))

(defun load-file-in-inf-ruby ()
  (interactive)
  (ruby-load-file (buffer-file-name))
  (ruby-switch-to-inf (get-buffer "*ruby*")))

(defun configure-ruby ()
  (require 'rvm)
  (require 'ruby-electric)
  (require 'ido)
  (require 'rinari)
  (require 'yari)
  (rvm-use-default)
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key (kbd "C-?") 'yari)
  (local-set-key (kbd "C-<f10>") 'run-ruby)
  (local-set-key (kbd "C-<f5>") 'load-file-in-inf-ruby)
  (local-set-key (kbd "C-<f8>") 'rinari-rake)
  (local-set-key (kbd "C-x T") 'rinari-run-all-test)
  (ruby-electric-mode t))

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(add-hook 'ruby-mode-hook 'configure-ruby)

(provide 'config-ruby)
