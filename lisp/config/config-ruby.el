;;; Configuration for Ruby

(require 'config-common)
(require 'rvm)

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

(defun run-ruby-debugger ()
  (interactive)
  (let* ((debug-command (concat  "ruby -r debug " buffer-file-name))
         (user-debug-command (read-string "Run ruby -r debug (like this): "
                                          debug-command)))
    (gud-gdb user-debug-command)))

(defconfig configure-ruby
  (require 'ruby-mode)
  (require 'ruby-electric)
  (require 'ido)
  (require 'rinari)
  (require 'yari)
  (smartparens-mode)
  (local-set-key (kbd "C-?") 'yari)
  (local-set-key (kbd "C-<f5>") 'load-file-in-inf-ruby)
  (local-set-key (kbd "C-x C-a C-a") 'load-file-in-inf-ruby)
  (local-set-key (kbd "C-x a a") 'load-file-in-inf-ruby)
  (local-set-key (kbd "C-<f10>") 'run-ruby)
  (local-set-key (kbd "C-! C-r") 'run-ruby)
  (local-set-key (kbd "C-<f7>") 'rinari-rake)
  (local-set-key (kbd "C-! C-a") 'rinari-rake)
  (local-set-key (kbd "C-x T") 'rinari-run-all-test)
  (local-set-key (kbd "C-<f11>") 'run-ruby-debugger)
  (local-set-key (kbd "C-! C-d") 'run-ruby-debugger)
  (ruby-electric-mode t))

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(add-hook 'ruby-mode-hook 'configure-ruby)

(rvm-use-default)

(provide 'config-ruby)
