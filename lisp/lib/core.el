;;; Core

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(eval-and-compile
  ;; modify GC limits for startup
  (defvar core--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6
          file-name-handler-alist nil))

  ;; be quiet at startup; don't load or display anything unnecessary
  (unless noninteractive
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (setq inhibit-startup-message t
          inhibit-startup-echo-area-message user-login-name
          inhibit-default-init t
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil
          mode-line-format nil)))

(defun core/finalize ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist core--file-name-handler-alist))

(add-hook 'emacs-startup-hook 'core/finalize)

(require 'package)

(defun core/is-windows? ()
  (equal system-type 'windows-nt))

(defun core/defsource (name-uri-cons)
  (add-to-list 'package-archives name-uri-cons t))

(defun core/initialize-packages ()
  (package-initialize)

  (when (or (not (package-installed-p 'use-package))
            (not (package-installed-p 'diminish)))
    (package-refresh-contents))
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (when (not (package-installed-p 'diminish))
    (package-install 'diminish))

  (eval-when-compile
    (require 'use-package))
  (require 'bind-key))

(provide 'core)
