;;; mod-lang-c.el --- Configuration for C/C++        -*- lexical-binding: t; -*-

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode))
  :bind (:map gud-minor-mode-map
         ("C-<f12>" . gdb-display-disassembly-buffer)
         ("C-c q" . gdb-display-disassembly-buffer))
  :lang (:map c-mode-map
         (:debugger . gdb)
         :map c++-mode-map
         (:debugger . gdb)
         :map gud-minor-mode-map
         (:eval-buffer . gud-run)
         (:debug-set-break . gud-break)
         (:debug-remove-break . gud-remove)
         (:debug-step-over . gud-next)
         (:debug-step-into . gud-step)
         (:debug-step-out . gud-finish)
         (:debug-continue . gud-cont)
         (:debug-run . gud-run))
  :config
  (setq c-default-style '((java-mode . "k&r")
                          (csharp-mode . "c#")
                          (awk-mode . "awk")
                          (other . "k&r")))

  (setq-default c-basic-offset 4))

(use-package c-eldoc
  :ensure t
  :after cc-mode
  :config
  (setq c-eldoc-includes
        "`pkg-config glib-2.0 gio-2.0 --cflags` `guile-config compile` -I/usr/include -I./ -I../ ")
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode))

(provide 'mod-lang-c)
