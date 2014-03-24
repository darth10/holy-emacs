;;; Configuration for sticky keys

(require 'sticky-control)

(add-to-list 'sticky-control-shortcuts '(?k . "\C-k"))
(add-to-list 'sticky-control-shortcuts '(?o . "\C-x\C-f"))
(add-to-list 'sticky-control-shortcuts '(?w . "\C-x\C-s"))
(add-to-list 'sticky-control-shortcuts '(?d . "\C-d"))

(setq sticky-control-timeout 0.4)

(defun set-sticky-mode (c)
  (setq sticky-control-key c)
  (sticky-control-mode t))

(provide 'config-sticky)
