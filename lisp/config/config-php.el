;;; Configuration for PHP

;; (require 'config-common)
(require 'config-gud)

(defun php-debug ()
  (interactive)
  (call-interactively 'geben)
  (shell-command
   (concat "XDEBUG_CONFIG='idekey=php-54' php "
           (buffer-file-name) " &")))

(defconfig configure-php
  ;; (smartparens-mode)
  (local-set-key (kbd "C-<f5>") 'php-debug)
  (local-set-key (kbd "C-x C-a C-a") 'php-debug)
  (local-set-key (kbd "C-x a a") 'php-debug)
  (local-set-key (kbd "C-! C-d") 'geben)
  (local-set-key (kbd "C-<f11>") 'geben))

(defun configure-geben
  (interactive)

  (define-key geben-mode-map (kbd config-key-gud-break1) 'geben-set-breakpoint-line)
  (define-key geben-mode-map (kbd config-key-gud-break2) 'geben-set-breakpoint-line)
  (define-key geben-mode-map (kbd config-key-gud-break3) 'geben-set-breakpoint-line)

  (define-key geben-mode-map (kbd config-key-gud-remove1) 'geben-unset-breakpoint-line)
  (define-key geben-mode-map (kbd config-key-gud-remove2) 'geben-unset-breakpoint-line)
  (define-key geben-mode-map (kbd config-key-gud-remove3) 'geben-unset-breakpoint-line)

  (define-key geben-mode-map (kbd config-key-gud-next1) 'geben-step-over)
  (define-key geben-mode-map (kbd config-key-gud-next2) 'geben-step-over)
  (define-key geben-mode-map (kbd config-key-gud-next3) 'geben-step-over)

  (define-key geben-mode-map (kbd config-key-gud-step1) 'geben-step-into)
  (define-key geben-mode-map (kbd config-key-gud-step2) 'geben-step-into)
  (define-key geben-mode-map (kbd config-key-gud-step3) 'geben-step-into)

  (define-key geben-mode-map (kbd config-key-gud-finish1) 'geben-step-out)
  (define-key geben-mode-map (kbd config-key-gud-finish2) 'geben-step-out)
  (define-key geben-mode-map (kbd config-key-gud-finish3) 'geben-step-out)

  (define-key geben-mode-map (kbd config-key-gud-cont1) 'geben-run)
  (define-key geben-mode-map (kbd config-key-gud-cont2) 'geben-run)
  (define-key geben-mode-map (kbd config-key-gud-cont3) 'geben-run))

(add-hook 'php-mode-hook 'configure-php)
(add-hook 'geben-session-enter-hook 'configure-geben)

(provide 'config-php)
