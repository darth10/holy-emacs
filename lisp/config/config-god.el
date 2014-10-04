;;; Configuration for God Mode

(require 'god-mode)

(defun set-god-mode (god-mode-key god-mode-all-key)
  (progn
    (let ((god-mode-key-kbd (kbd god-mode-key))
          (god-mode-all-key-kbd (kbd god-mode-all-key)))
      (global-set-key god-mode-key-kbd 'god-local-mode)
      (global-set-key god-mode-all-key-kbd 'god-mode-all))
    (add-to-list 'god-exempt-major-modes 'dired-mode)
    (define-key god-local-mode-map (kbd ".") 'repeat)
    (define-key god-local-mode-map (kbd "z") 'repeat)
    (define-key god-local-mode-map (kbd "i") 'god-local-mode)
    (god-mode)))

(defun god-toggle-on-overwrite ()
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)

(provide 'config-god)
