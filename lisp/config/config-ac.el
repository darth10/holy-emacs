;;; Configuration for auto-complete

(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/data/ac-dict")
(ac-config-default)
(setq ac-delay 0.3)
(global-auto-complete-mode t)

(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

(global-unset-key (kbd "M-SPC"))
(global-set-key (kbd "M-SPC") 'auto-complete)
(global-set-key (kbd "C-' C-' C-a") 'auto-complete-mode)
(global-set-key (kbd "C-' ' a") 'auto-complete-mode)

(provide 'config-ac)
