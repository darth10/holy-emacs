;;; core.el -*- lexical-binding: t; -*-

;;; Emacs Lisp naming conventions:
;;
;; Definitions in files within the core/ directory should use
;; the following conventions:
;; * core/...      interactive functions
;; * core:...      public non-interactive functions
;; * core--...     any non-interactive private definitions
;; * core-...      any other public definitions like vars, consts, etc
;;
;; Definitions in files outside the core/ directory should
;; have the +file... prefix and use the same conventions:
;; * +module/...   interactive functions
;; * +module:...   public non-interactive functions
;; * +module--...  any non-interactive private definitions
;; * +module-...   any other public definitions like vars, consts, etc
;;
;; The only exceptions to these conventions are:
;; * the holy-emacs customization group (core.el)
;; * the holy-emacs-version const       (core.el)

;;; Key binding conventions:
;;
;; TODO
;; based on https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

(eval-and-compile
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8-unix)
  (set-default-coding-systems  'utf-8-unix)
  (set-terminal-coding-system  'utf-8-unix)
  (set-keyboard-coding-system  'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (setq locale-coding-system   'utf-8-unix)

  (setq-default buffer-file-coding-system 'utf-8-unix
                default-buffer-file-coding-system 'utf-8-unix
                delete-trailing-lines t
                indent-tabs-mode nil
                tab-always-indent t
                tab-width 4
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
          gc-cons-percentage 0.7
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

  (require 'cl-lib)
  (require 'package)

  (defun core--finalize-startup ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist core--file-name-handler-alist))

  (add-hook 'emacs-startup-hook #'core--finalize-startup))

(defgroup holy-emacs nil
  "An opinionated and extensible Emacs configuration"
  :group 'emacs)

(defconst holy-emacs-version "0.1.1"
  "Version of holy-emacs.")

(defconst core-modules-path
  "lisp/config/"
  "Relative path of all modules.")

(defconst core-var-dir-path
  "lisp/var/"
  "Relative path of all custom Emacs Lisp files.")

(defconst core--required-packages
  '(use-package diminish epl async)
  "List of required packages.")

(defconst core--elisp-dir-paths
  (list "elpa" "lisp/lib/" core-modules-path core-var-dir-path)
  "List of relative paths containing Emacs Lisp files
for byte compilation.")

(defconst core-custom-defs-file-path
  (concat user-emacs-directory core-var-dir-path "custom-defs.el")
  "Absolute path to save customize definitions.")

(defun core:is-windows-p ()
  "Checks if the current OS is Windows."
  (equal system-type 'windows-nt))

(defun core:defsource (name-uri-cons)
  "Add a source name and URI pair NAME-URI-CONS to the list of package sources."
  (add-to-list 'package-archives name-uri-cons t))

(defun core--is-package-not-installed-p (pkg)
  "Checks if package PKG needs to be installed."
  (not (package-installed-p pkg)))

(defun core:compile-file (file)
  "Compile/recompile an Emacs Lisp file."
  (if (file-exists-p (concat file "c"))
      (byte-recompile-file file)
    (byte-compile-file file)))

(defun core--load-var-dir ()
  "Loads all Emacs Lisp files from directory CORE-VAR-DIR-PATH."
  (let* ((var-dir (concat user-emacs-directory core-var-dir-path))
         (files (directory-files var-dir))
         (file-names (mapcar 'file-name-base files))
         (dup-f (lambda (x y) (equal x y)))
         (filter-f (lambda (x)
                     (or (equal x ".")
                         (equal x ".gitignore"))))
         (packages (remove-duplicates (cl-remove-if filter-f file-names)
                                      :test dup-f)))
    (cl-loop for pkg in packages
             collect pkg
             and do (load pkg))))

(defun core/initialize-packages ()
  "Initializes the package sub-system."
  (package-initialize)

  (when (cl-some #'core--is-package-not-installed-p
              core--required-packages)
    (package-refresh-contents))

  (cl-loop for pkg in core--required-packages
           if (core--is-package-not-installed-p pkg)
           collect pkg
           and do (package-install pkg))

  ;; require only a few packages here
  ;; and the rest when they're needed
  (eval-when-compile
    (require 'use-package))
  (require 'bind-key))

(defun core/initialize-modules ()
  "Initializes global load path and module sub-system."
  (setq custom-file core-custom-defs-file-path)
  ;; set load-path
  (cl-loop for path in (core--get-elisp-dirs)
           collect path
           and do (add-to-list 'load-path path))

  (require 'core-keys)
  (require 'core-extensions)

  (add-hook 'after-init-hook #'core--load-var-dir)
  (advice-add 'customize-save-variable :after
              #'(lambda (_variable _value)
                  (core:compile-file core-custom-defs-file-path))))

(defun core/upgrade-packages ()
  "Upgrade all packages."
  (interactive)
  (require 'epl)
  (epl-refresh)
  (epl-upgrade))

(defun core--get-elisp-dirs ()
  "Get a list of absolute paths of directories containing Emacs
Lisp files for byte compilation."
  (cl-loop for dir in core--elisp-dir-paths
           collect (concat user-emacs-directory dir)))

(defun core/autoremove-packages ()
  "Delete unused packages."
  (interactive)
  (package--save-selected-packages (package--find-non-dependencies))
  (package-autoremove))

(defun core/byte-recompile-files ()
  "Recompile all Emacs Lisp files."
  (interactive)
  (cl-loop for path in (core--get-elisp-dirs)
           collect path
           and do (byte-recompile-directory (expand-file-name path) 0)))

(defun core/clean-byte-compiled-files ()
  "Delete all compiled Emacs Lisp files."
  (interactive)
  (let* ((recursive-elc-files
          (cl-loop for dir in (core--get-elisp-dirs)
                   collect (directory-files-recursively dir "\\.elc$")))
         (elc-files (apply #'append recursive-elc-files)))
    (unless (cl-loop for path in elc-files
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path))
      (message "Removed all .elc files"))))

(provide 'core)
