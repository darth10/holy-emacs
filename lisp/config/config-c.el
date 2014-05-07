;;; Configuration for C/C++

(require 'config-common)
(require 'config-gud)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(defun config-display-gdb-buffer ()
  (interactive)
  (if (fboundp 'gdb-display-gdb-buffer)
      (gdb-display-gdb-buffer)
    (config-show-no-gud)))

(defconfig configure-c
  (setq c-eldoc-includes "`pkg-config glib-2.0 gio-2.0 --cflags` `guile-config compile` -I/usr/include -I./ -I../ ")
  (load "c-eldoc")
  (c-turn-on-eldoc-mode)
  (local-set-key (kbd "C-<f10>") 'config-display-gdb-buffer)
  (local-set-key (kbd "C-! C-r") 'config-display-gdb-buffer)
  (local-set-key (kbd "C-c C-z") 'config-display-gdb-buffer)
  (local-set-key (kbd "C-<f11>") 'gdb)
  (local-set-key (kbd "C-! C-d") 'gdb)
  (local-set-key (kbd "C-<f5>") 'gud-run)
  (local-set-key (kbd "C-x C-a C-a") 'gud-run)
  (local-set-key (kbd "C-x a a") 'gud-run)
  (local-set-key (kbd "C-<f12>") 'gdb-display-disassembly-buffer)
  (local-set-key (kbd "C-x C-a C-q") 'gdb-display-disassembly-buffer)
  (local-set-key (kbd "C-x a q") 'gdb-display-disassembly-buffer))

(add-hook 'c-mode-hook 'configure-c)
(add-hook 'c++-mode-hook 'configure-c)

(provide 'config-c)
