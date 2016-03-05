;;; Configuration for Ruby

(use-package ruby-mode
  :ensure t
  :mode ("\\.rb\\'" . ruby-mode))

(use-package inf-ruby
  :ensure t
  :bind (:map ruby-mode-map
         ("C-! C-r" . run-ruby)
         ("C-<f10>" . run-ruby)
         ("C-x C-a C-a" . load-file-in-inf-ruby)
         ("C-x a a" . load-file-in-inf-ruby)
         ("C-<f5>" . load-file-in-inf-ruby)
         ("C-! C-d" . run-ruby-debugger)
         ("C-<f11>" . run-ruby-debugger))
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
      (gud-gdb user-debug-command))))

(use-package ruby-end
  :ensure t
  :diminish ruby-end-mode)

(use-package rinari
  :ensure t
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
  :bind (:map ruby-mode-map
         ("C-?" . yari)))

(use-package rvm
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'rvm-use-default))

(provide 'config-ruby)
