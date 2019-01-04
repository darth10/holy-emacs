;;; Core -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'package)

(defun core/is-windows? ()
  "Checks if the current OS is Windows"
  (equal system-type 'windows-nt))

(defun core/defsource (name-uri-cons)
  "Add a source name and URI pair NAME-URI-CONS to the list of package sources"
  (add-to-list 'package-archives name-uri-cons t))

(defconst core--required-packages
  '(use-package diminish epl async))

(defun core--is-package-not-installed? (pkg)
  (not (package-installed-p pkg)))

(defun core/initialize-packages ()
  "Initialize package system"
  (package-initialize)

  (when (cl-some #'core--is-package-not-installed?
			  core--required-packages)
    (package-refresh-contents))

  (cl-loop for pkg in core--required-packages
		   if (core--is-package-not-installed? pkg)
  		   collect pkg
  		   and do (package-install pkg))

  ;; require only a few packages here
  ;; and the rest when they're needed
  (eval-when-compile
    (require 'use-package))
  (require 'bind-key))

(defun core/upgrade-packages ()
  "Upgrade all packages"
  (interactive)
  (require 'epl)
  (epl-refresh)
  (epl-upgrade))

(defconst core--elisp-dir-paths
  (list "elpa"
		"lisp/lib"
		"lisp/config"))

(defun core--get-elisp-dirs ()
  (mapcar (lambda (x) (concat user-emacs-directory x))
		  core--elisp-dir-paths))

(defun core/set-load-path ()
  "Adds directories with Emacs Lisp files to the global load path"
  (cl-loop for path in (core--get-elisp-dirs)
		   collect path
		   and do (add-to-list 'load-path path)))

(defun core/autoremove-packages ()
  "Delete unused packages"
  (package--save-selected-packages (package--find-non-dependencies))
  (package-autoremove))

(defun core/byte-recompile-files ()
  "Recompile all Emacs Lisp files"
  (interactive)
  (cl-loop for path in (core--get-elisp-dirs)
		   collect path
		   and do (byte-recompile-directory (expand-file-name path) 0)))

(defun core/clean-byte-compiled-files ()
  "Delete all compiled Emacs Lisp files"
  (interactive)
  (let* ((recursive-elc-files (mapcar (lambda (x) (directory-files-recursively x "\\.elc$"))
									  (core--get-elisp-dirs)))
		 (elc-files (apply #'append recursive-elc-files)))
    (unless (cl-loop for path in elc-files
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path))
      (message "Removed all .elc files"))))

(provide 'core)
