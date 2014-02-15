;;; Configuration for GUD

(require 'config-common)
(require 'gud)

(defun config-show-no-gud ()
  (message "No active debug buffer"))

(defmacro global-gud-set-key (key fn)
  `(global-for-key (kbd ,key)
                   (if gud-minor-mode (,fn 1)
                     (config-show-no-gud))))

(global-gud-set-key "<f8>" gud-run)
(global-gud-set-key "C-x C-a r" gud-run)
(global-gud-set-key "<f9>" gud-break)
(global-gud-set-key "S-<f9>" gud-remove)
(global-gud-set-key "<f10>" gud-next)
(global-gud-set-key "<f11>" gud-step)
(global-gud-set-key "S-<f11>" gud-finish)
(global-gud-set-key "<f5>" gud-cont)


(setq gdb-many-windows t)

(provide 'config-gud)
