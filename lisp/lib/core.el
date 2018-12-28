;;; Core

(eval-and-compile
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system   'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  (setq-default tab-width 4)

  (setq-default
   left-fringe-width 8
   right-fringe-width 8
   frame-inhibit-implied-resize t
   window-divider-default-places t
   window-divider-default-bottom-width 0
   window-divider-default-right-width 1
   ;; JIT tweaks for font performance
   jit-lock-defer-time nil
   jit-lock-stealth-nice 0.1
   jit-lock-stealth-time 0.2
   jit-lock-stealth-verbose nil)

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
          mode-line-format nil))

  (defun core--finalize-startup ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist core--file-name-handler-alist))

  (add-hook 'emacs-startup-hook 'core--finalize-startup))

(require 'package)

(defun core/is-windows? ()
  "Checks if the current OS is Windows"
  (equal system-type 'windows-nt))

(defun core/defsource (name-uri-cons)
  "Add a source name and URI pair NAME-URI-CONS to the list of package sources"
  (add-to-list 'package-archives name-uri-cons t))

(defun core/initialize-packages ()
  "Initialize package system"
  (package-initialize)

  (when (or (not (package-installed-p 'use-package))
            (not (package-installed-p 'diminish))
            (not (package-installed-p 'epl)))
    (package-refresh-contents))
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (when (not (package-installed-p 'diminish))
    (package-install 'diminish))
  (when (not (package-installed-p 'epl))
    (package-install 'epl))

  (eval-when-compile
    (require 'use-package))
  (require 'bind-key))

(defun core/upgrade-packages ()
  "Upgrade all packages"
  (interactive)
  (require 'epl)
  (epl-refresh)
  (epl-upgrade))

(defun core/autoremove-packages ()
  "Delete unused packages"
  (package--save-selected-packages (package--find-non-dependencies))
  (package-autoremove))

(defun core/byte-recompile-files ()
  "Recompile all .el files"
  (interactive)
  (let ((targets (list "~/.emacs.d/elpa"
					   "~/.emacs.d/lisp/lib"
					   "~/.emacs.d/lisp/config")))
	(cl-loop for path in targets
			 collect path
			 and do (byte-recompile-directory (expand-file-name path) 0))))

(defun core/clean-byte-compiled-files ()
  "Delete all compiled .elc files"
  (interactive)
  (let ((targets (append (directory-files-recursively "~/.emacs.d/elpa" "\\.elc$")
						 (directory-files-recursively "~/.emacs.d/lisp/lib" "\\.elc$")
                         (directory-files-recursively "~/.emacs.d/lisp/config" "\\.elc$"))))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path))
      (message "Removed all .elc files"))))

(provide 'core)
