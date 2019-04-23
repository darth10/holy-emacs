;;; Configuration for PHP

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

(use-package geben
  :ensure t
  :defer 5
  :init
  (setq geben-temporary-file-directory
        (concat core-var-cache-dir-full-path "geben/"))
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

    (defun php-debug-sentinel (process event)
      (when (= 0 (process-exit-status process))
        (call-interactively 'geben-end)))

    (defun php-debug ()
      (interactive)
      (call-interactively 'geben)
      (let* ((php-debug-command (concat
                                 "XDEBUG_CONFIG='idekey=php-54' php "
                                 (buffer-file-name)))
             (php-debug-process (start-process
                                 "php-debug-process"
                                 "*php-debug-process-output*"
                                 "/bin/sh" "-c"
                                 php-debug-command)))
        (set-process-sentinel php-debug-process 'php-debug-sentinel)))

    (bind-key "C-! C-r" 'php-debug php-mode-map)
    (bind-key "C-<f10>" 'php-debug php-mode-map)

    (bind-key "C-! C-d" 'geben php-mode-map)
    (bind-key "C-<f11>" 'geben php-mode-map)))

(provide 'mod-lang-php)
