;;; mod-lang-c.el --- Configuration for C/C++        -*- lexical-binding: t; -*-

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode))
  :bind (:map gud-minor-mode-map
         ("C-<f12>" . gdb-display-disassembly-buffer)
         ("C-c q" . gdb-display-disassembly-buffer))
  :lang (:map c-mode-map
         (:repl-start . config-display-gdb-buffer)
         (:eval-buffer . gud-run)
         (:debugger . gdb))
  :config

  (defun config-display-gdb-buffer ()
    (interactive)
    (when (fboundp 'gdb-display-gdb-buffer)
        (gdb-display-gdb-buffer)))

  (setq c-default-style '((java-mode . "k&r")
                          (csharp-mode . "c#")
                          (awk-mode . "awk")
                          (other . "k&r")))

  (setq-default c-basic-offset 4))

(use-package gud
  :defer 2
  :config
  (setq gdb-many-windows t)

  (core-bind-keys core-lang-debug-set-breakpoint-keys #'gud-break 'gud-minor-mode-map)
  (core-bind-keys core-lang-debug-remove-breakpoint-keys #'gud-remove 'gud-minor-mode-map)
  (core-bind-keys core-lang-debug-step-over-keys #'gud-next 'gud-minor-mode-map)
  (core-bind-keys core-lang-debug-step-into-keys #'gud-step 'gud-minor-mode-map)
  (core-bind-keys core-lang-debug-step-out-keys #'gud-finish 'gud-minor-mode-map)
  (core-bind-keys core-lang-debug-continue-keys #'gud-cont 'gud-minor-mode-map)
  (core-bind-keys core-lang-debug-run-keys #'gud-run 'gud-minor-mode-map))

(use-package c-eldoc
  :ensure t
  :defer 5
  :config
  (setq c-eldoc-includes "`pkg-config glib-2.0 gio-2.0 --cflags` `guile-config compile` -I/usr/include -I./ -I../ ")
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(provide 'mod-lang-c)
