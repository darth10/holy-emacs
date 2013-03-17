;;; Configuration for C

(setq gdb-many-windows t)

;; eldoc
(setq c-eldoc-includes "`pkg-config glib-2.0 gio-2.0 --cflags` `guile-config compile` -I/usr/include -I./ -I../ ")
(load "c-eldoc")

(defun configure-c ()
  (c-turn-on-eldoc-mode)
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key (kbd "C-<f10>") 'gdb)
  (local-set-key (kbd "C-<f5>") 'gud-run)
  (local-set-key (kbd "C-<f11>") 'gdb-display-gdb-buffer)
  (local-set-key (kbd "C-<f12>") 'gdb-display-disassembly-buffer)
  (local-set-key (kbd "<f5>") 'gud-step)
  (local-set-key (kbd "<f6>") 'gud-next)
  (local-set-key (kbd "<f7>") 'gud-finish)
  (local-set-key (kbd "<f8>") 'gud-cont))

(add-hook 'c-mode-hook 'configure-c)

(provide 'config-c)
