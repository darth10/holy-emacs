;;; Configuration for C

(setq gdb-many-windows t)

;; eldoc
(setq c-eldoc-includes "`pkg-config glib-2.0 gio-2.0 --cflags` -I/usr/include -I./ -I../ ")
(load "c-eldoc")

(defun configure-c ()
  (c-turn-on-eldoc-mode)
  (local-set-key (kbd "C-x <f10>") 'gdb)
  (local-set-key (kbd "C-x <f5>") 'gud-run)
  (local-set-key (kbd "<f5>") 'gud-step)
  (local-set-key (kbd "<f6>") 'gud-next)
  (local-set-key (kbd "<f7>") 'gud-finish)
  (local-set-key (kbd "<f8>") 'gud-cont)
  (local-set-key (kbd "<f11>") 'gdb-display-gdb-buffer)
  (local-set-key (kbd "<f12>") 'gdb-display-disassembly-buffer))

(add-hook 'c-mode-hook 'configure-c)

(provide 'config-c)
