;;; mod-lang-ruby.el --- Configuration for Ruby      -*- lexical-binding: t; -*-

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode))

(use-package ruby-end
  :ensure t
  :after ruby-mode)

(use-package inf-ruby
  :ensure t
  :after ruby-mode
  :lang (:map ruby-mode-map
         (:repl-start . run-ruby)
         (:eval-buffer . +ruby/load-file)
         (:load-file . +ruby/load-file)
         (:debugger . +ruby/run-debugger))
  :config
  (defun +ruby/load-file ()
    (interactive)
    (ruby-load-file (buffer-file-name))
    (ruby-switch-to-inf (get-buffer "*ruby*")))
  (defun +ruby/run-debugger ()
    (interactive)
    (let* ((debug-command (concat  "ruby -r debug " buffer-file-name))
           (user-debug-command (read-string "Run ruby -r debug (like this): "
                                            debug-command)))
      (gud-gdb user-debug-command))))

(use-package robe
  :ensure t
  :defer 2
  :lang (:comp (ruby-mode . company-robe)
         :map ruby-mode-map
         (:find-definition . robe-jump))
  :config
  (add-hook 'ruby-mode-hook #'robe-mode))

(use-package rinari
  :ensure t
  :defer 2
  :lang (:map ruby-mode-map
         (:test-all . rinari-test)))

(use-package rvm
  :ensure t
  :defer 2)

(provide 'mod-lang-ruby)
