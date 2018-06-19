;;; Configuration for Ruby

(use-package ruby-mode
  :ensure t
  :mode ("\\.rb\\'" . ruby-mode))

(use-package inf-ruby
  :ensure t
  :defer 5
  :config
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
  (bind-key "C-! C-r" 'run-ruby ruby-mode-map)
  (bind-key "C-<f10>" 'run-ruby ruby-mode-map)
  (bind-key "C-x C-a C-a" 'load-file-in-inf-ruby ruby-mode-map)
  (bind-key "C-x a a" 'load-file-in-inf-ruby ruby-mode-map)
  (bind-key "C-<f5>" 'load-file-in-inf-ruby ruby-mode-map)
  (bind-key "C-! C-d" 'run-ruby-debugger ruby-mode-map)
  (bind-key "C-<f11>" 'run-ruby-debugger ruby-mode-map))

(use-package ruby-end
  :ensure t
  :defer 5
  :diminish ruby-end-mode)

(use-package rinari
  :ensure t
  :defer 5
  :bind (:map ruby-mode-map
         ("C-! C-a" . rinari-rake)
         ("C-<f7>" . rinari-rake))
  :config
  (defun rinari-run-all-test ()
    (interactive)
    (rinari-rake "test"))
  (bind-key "C-x T" 'rinari-run-all-test ruby-mode-map))

(use-package yari
  :ensure t
  :defer 5
  :bind (:map ruby-mode-map
         ("C-?" . yari)))

(use-package rvm
  :ensure t
  :defer 5
  :config
  (add-hook 'ruby-mode-hook 'rvm-use-default))

(provide 'config-ruby)
