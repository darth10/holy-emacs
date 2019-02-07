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
;; * the `holy-emacs' customization group (core.el)
;; * the `holy-emacs-version' const       (core.el)

;;; Key binding conventions:
;;
;; TODO
;; based on https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

(defgroup holy-emacs nil
  "An opinionated and extensible Emacs configuration."
  :group 'emacs)

(defconst holy-emacs-version "0.1.6"
  "Version of holy-emacs.")

(eval-and-compile
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8-unix)
  (set-default-coding-systems  'utf-8-unix)
  (set-terminal-coding-system  'utf-8-unix)
  (set-keyboard-coding-system  'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (setq locale-coding-system   'utf-8-unix)

  (setq-default
   buffer-file-coding-system 'utf-8-unix
   default-buffer-file-coding-system 'utf-8-unix)

  ;; Increase GC limits and remove file handlers for startup
  (defvar core--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.7
          file-name-handler-alist nil))

  ;; Don't load or display anything unnecessary during startup
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
    ;; Reset GC limits and file handlers
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist core--file-name-handler-alist))

  (add-hook 'emacs-startup-hook #'core--finalize-startup))

(defconst core-lib-path "lisp/lib/"
  "Relative path of all modules.")

(defconst core-modules-lib-path "lisp/config/"
  "Relative path of all modules.")

(defconst core-var-dir-path "var/"
  "Relative path of directory containing all files that are
prone to change.")

(defconst core-var-lib-path "lisp/var/"
  "Relative path of directory containing Emacs Lisp files that are
prone to change.")

(defconst core-elpa-packages-path
  (concat core-var-dir-path "packages/elpa/")
  "Relative path of ELPA/MELPA packages directory.")

(defconst core-quelpa-packages-path
  (concat core-var-dir-path "packages/quelpa/")
  "Relative path of quelpa packages directory.")

(defconst core--required-packages
  '(epl async quelpa use-package quelpa-use-package)
  "List of required packages.")

(defun core--get-elisp-compile-dirs ()
  "Get a list of absolute paths of directories containing Emacs
Lisp files for byte compilation."
  (cl-loop for dir in (list core-lib-path
                            core-modules-lib-path
                            core-var-lib-path
                            core-elpa-packages-path)
           collect (expand-file-name dir user-emacs-directory)))

(defun core--init-load-path ()
  "Adds required paths to the `load-path' variable."
  (cl-loop for path in (list core-lib-path
                             core-modules-lib-path
                             core-var-lib-path)
           collect path
           and do (add-to-list
                   'load-path
                   (expand-file-name path user-emacs-directory))))

(defun core--load-var-dir ()
  "Loads all Emacs Lisp files from directory `core-var-lib-path'."
  (let* ((var-dir (concat user-emacs-directory core-var-lib-path))
         (files (directory-files var-dir))
         (file-names (cl-loop for file in files
                              collect (file-name-base file)))
         (packages (cl-remove-duplicates
                    (cl-remove-if
                     (lambda (x)
                       (or (equal x ".")
                           (equal x ".gitignore")))
                     file-names)
                    :test #'equal)))
    (cl-loop for pkg in packages
             collect pkg
             and do (load pkg))))

(defun core--check-and-install-required-packages ()
  "Checks if packages in `core--required-packages' are installed
and installs them if needed. Must be called after
`package-initialize'."
  (cl-flet ((to-install-package-p (pkg)
                                  (not (package-installed-p pkg))))
    (when (cl-some #'to-install-package-p
                   core--required-packages)
      (package-refresh-contents))

    (cl-loop for pkg in core--required-packages
             if (to-install-package-p pkg)
             collect pkg
             and do (package-install pkg))))

(defun core:is-windows-p ()
  "Checks if the current OS is Windows."
  (equal system-type 'windows-nt))

(defun core:defsource (name-uri-cons)
  "Add a source name and URI pair NAME-URI-CONS to the list of package sources."
  (add-to-list 'package-archives name-uri-cons t))

(defun core:compile-file (file)
  "Compile/recompile an Emacs Lisp file."
  (if (file-exists-p (concat file "c"))
      (byte-recompile-file file)
    (byte-compile-file file)))

(defun core:initialize-packages-and-modules ()
  "Initializes the packages and modules sub-system."
  (setq package-user-dir (expand-file-name
                          core-elpa-packages-path user-emacs-directory)
        package-gnupghome-dir (expand-file-name
                               "gnupg/" package-user-dir)
        quelpa-dir (expand-file-name
                    core-quelpa-packages-path user-emacs-directory)
        quelpa-checkout-melpa-p nil
        quelpa-update-melpa-p nil
        quelpa-melpa-recipe-stores nil
        quelpa-self-upgrade-p nil
        quelpa-use-package-inhibit-loading-quelpa t)
  (package-initialize)
  (core--check-and-install-required-packages)

  ;; Require only a few packages here and the rest when they're
  ;; needed. They should be available on the `load-path' as
  ;; `package-initialize' has been called.
  (eval-when-compile
    (require 'use-package))
  (require 'quelpa-use-package)
  (require 'bind-key)
  (core--init-load-path)

  (require 'core-keys)
  (require 'core-customize)
  (require 'core-ui)
  (require 'core-extensions)
  ;; core-editor.el is not loaded here as it's not required.
  (core--load-var-dir))

(defun core/upgrade-packages ()
  "Upgrade all packages."
  (interactive)
  (require 'epl)
  (epl-refresh)
  (quelpa-upgrade)
  (epl-upgrade)
  (message "Upgraded all packages!"))

(defun core/autoremove-packages ()
  "Delete unused packages."
  (interactive)
  (package--save-selected-packages (package--find-non-dependencies))
  (package-autoremove))

(defun core/byte-recompile-files ()
  "Recompile all Emacs Lisp files."
  (interactive)
  (cl-loop for dir in (core--get-elisp-compile-dirs)
           collect dir
           and do (byte-recompile-directory dir 0)))

(defun core/clean-byte-compiled-files ()
  "Delete all compiled Emacs Lisp files."
  (interactive)
  (let* ((recursive-elc-files
          (cl-loop for dir in (core--get-elisp-compile-dirs)
                   if (file-exists-p dir)
                   collect (directory-files-recursively dir "\\.elc$")))
         (elc-files (apply #'append recursive-elc-files)))
    (if (cl-loop for path in elc-files
                 if (file-exists-p path)
                 collect path
                 and do
                 (delete-file path)
                 (message "Deleted %s" path))
        (message "Deleted all .elc files!")
      (message "No .elc files found!"))))

(provide 'core)
