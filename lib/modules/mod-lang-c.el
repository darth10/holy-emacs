;;; mod-lang-c.el --- Configuration for C/C++        -*- lexical-binding: t; -*-

(use-package cc-mode
  :straight nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode))
  :lang (:map c-mode-map
         (:debugger . gdb)
         :map c++-mode-map
         (:debugger . gdb))
  :bind (:map gud-minor-mode-map
         ("C-<f12>" . gdb-display-disassembly-buffer)
         ("C-c q" . gdb-display-disassembly-buffer))
  :config
  (use-package gud)
  (setq c-default-style '((java-mode . "k&r")
                          (csharp-mode . "c#")
                          (awk-mode . "awk")
                          (other . "k&r")))

  (setq-default c-basic-offset 4))

(use-package c-eldoc
  :after cc-mode
  :hook (c-mode . c-turn-on-eldoc-mode)
  :config
  (setq c-eldoc-includes
        "`pkg-config glib-2.0 gio-2.0 --cflags` `guile-config compile` -I/usr/include -I./ -I../ "))

(provide 'mod-lang-c)
