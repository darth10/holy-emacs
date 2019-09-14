;;; mod-lang-php.el --- Configuration for PHP        -*- lexical-binding: t; -*-

;; To debug PHP scripts, start debugger using M-x geben and run:
;; php -S localhost:8081
;;     -d xdebug.remote_enable=on \
;;     -d xdebug.remote_host=127.0.0.1 \
;;     -d xdebug.remote_port=9000 \
;;     -d xdebug.remote_handler=dbgp \
;;     -d xdebug.idekey=geben \
;;     -d xdebug.remote_autostart=On \
;;     public/index.php

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package company-php
  :after php-mode
  :lang (:comp (php-mode . company-ac-php-backend)))

(use-package geben
  :after php-mode
  :lang (:map php-mode-map
         (:debugger . geben)
         :map geben-mode-map
         (:debug-set-break . geben-set-breakpoint-line)
         (:debug-remove-break . geben-unset-breakpoint-line)
         (:debug-step-over . geben-step-over)
         (:debug-step-into . geben-step-into)
         (:debug-step-out . geben-step-out)
         (:debug-continue . geben-run))
  :init
  (setq geben-temporary-file-directory
        (concat core-var-cache-dir-full-path "geben/")))

(provide 'mod-lang-php)
