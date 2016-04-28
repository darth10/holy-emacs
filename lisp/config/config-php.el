;;; Configuration for PHP

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

(use-package geben
  :ensure t
  :config

  (defun configure-geben (s)

    (bind-key config-key-gud-break1 'geben-set-breakpoint-line geben-mode-map)
    (bind-key config-key-gud-break2 'geben-set-breakpoint-line geben-mode-map)
    (bind-key config-key-gud-break3 'geben-set-breakpoint-line geben-mode-map)

    (bind-key config-key-gud-remove1 'geben-unset-breakpoint-line geben-mode-map)
    (bind-key config-key-gud-remove2 'geben-unset-breakpoint-line geben-mode-map)
    (bind-key config-key-gud-remove3 'geben-unset-breakpoint-line geben-mode-map)

    (bind-key config-key-gud-next1 'geben-step-over geben-mode-map)
    (bind-key config-key-gud-next2 'geben-step-over geben-mode-map)
    (bind-key config-key-gud-next3 'geben-step-over geben-mode-map)

    (bind-key config-key-gud-step1 'geben-step-into geben-mode-map)
    (bind-key config-key-gud-step2 'geben-step-into geben-mode-map)
    (bind-key config-key-gud-step3 'geben-step-into geben-mode-map)

    (bind-key config-key-gud-finish1 'geben-step-out geben-mode-map)
    (bind-key config-key-gud-finish2 'geben-step-out geben-mode-map)
    (bind-key config-key-gud-finish3 'geben-step-out geben-mode-map)

    (bind-key config-key-gud-cont1 'geben-run geben-mode-map)
    (bind-key config-key-gud-cont2 'geben-run geben-mode-map)
    (bind-key config-key-gud-cont3 'geben-run geben-mode-map))

  (add-hook 'geben-session-enter-hook 'configure-geben)

  (use-package php-mode
    :config

    (defun php-debug ()
      (interactive)
      (call-interactively 'geben)
      (let* ((php-debug-command (concat "XDEBUG_CONFIG='idekey=php-54' php "
                                        (buffer-file-name))))
        (start-process
         "php-debug-process"
         "*php-debug-process-output*"
         "/bin/sh" "-c"
         php-debug-command)))

    (bind-key "C-! C-r" 'php-debug php-mode-map)
    (bind-key "C-<f10>" 'php-debug php-mode-map)

    (bind-key "C-! C-d" 'geben php-mode-map)
    (bind-key "C-<f11>" 'geben php-mode-map)))

(provide 'config-php)
