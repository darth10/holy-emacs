;;; Configuration for sticky keys

(require 'sticky-control)

(setq sticky-control-timeout 0.4)

(defun set-sticky-mode (c)
  (setq sticky-control-key c)
  (sticky-control-mode t))

(provide 'config-sticky)
