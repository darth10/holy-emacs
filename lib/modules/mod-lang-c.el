;;; Configuration for C/C++

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode))

  :bind (:map c-mode-map
         ("C-<f10>" . config-display-gdb-buffer)
         ("C-! C-r" . config-display-gdb-buffer)
         ("C-c C-z" . config-display-gdb-buffer)
         ("C-<f11>" . gdb)
         ("C-! C-d" . gdb)
         ("C-<f5>" . gud-run)
         ("C-x C-a C-a" . gud-run)
         ("C-x a a" . gud-run)
         ("C-<f12>" . gdb-display-disassembly-buffer)
         ("C-x C-a C-q" . gdb-display-disassembly-buffer)
         ("C-x a q" . gdb-display-disassembly-buffer))
  :config

  (defun config-display-gdb-buffer ()
    (interactive)
    (when (fboundp 'gdb-display-gdb-buffer)
        (gdb-display-gdb-buffer)))

  (setq c-default-style '((java-mode . "k&r")
                          (csharp-mode . "c#")
                          (awk-mode . "awk")
                          (other . "k&r")
                          ))

  (setq-default c-basic-offset 4))

(use-package c-eldoc
  :ensure t
  :defer 5
  :config
  (setq c-eldoc-includes "`pkg-config glib-2.0 gio-2.0 --cflags` `guile-config compile` -I/usr/include -I./ -I../ ")
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(provide 'mod-lang-c)
