;;; core.el --- holy-emacs core                      -*- lexical-binding: t; -*-

(defgroup holy-emacs nil
  "An opinionated and extensible Emacs configuration."
  :group 'emacs)

(defconst holy-emacs-version "0.4.0"
  "Version of holy-emacs.")

(defconst core-lib-path "lib/core/"
  "Relative path of core files.")

(defconst core-modules-lib-path "lib/modules/"
  "Relative path of all module files.")

(defconst core-var-dir-path "var/"
  "Relative path of all files that are prone to change.")

(defconst core-var-lib-path "var/lib/"
  "Relative path of Emacs Lisp files that are prone to change.")

(defconst core-var-cache-dir-path
  (concat core-var-dir-path "cache/")
  "Relative path of all cached data.")

(defconst core-var-cache-dir-full-path
  (expand-file-name core-var-cache-dir-path user-emacs-directory)
  "Absolute path of all cached data.")

(defconst core-default-user-dir "~/.holy-emacs.d/"
  "Default absolute path of user configuration Emacs Lisp files.")

(defvar core-user-dir (or (getenv "HOLY_EMACS_HOME")
                          (and (file-directory-p core-default-user-dir)
                               core-default-user-dir))
  "Absolute path of user configuration Emacs Lisp files.")

(defconst core-packages-path (concat core-var-dir-path "packages/")
  "Relative path of installed packages.")

(defconst core--required-packages '(async use-package)
  "List of required packages.")

(eval-and-compile
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (set-selection-coding-system 'utf-8-unix)
  (set-default-coding-systems  'utf-8-unix)
  (set-terminal-coding-system  'utf-8-unix)
  (set-keyboard-coding-system  'utf-8-unix)
  (setq locale-coding-system   'utf-8-unix)
  (prefer-coding-system        'utf-8-unix)

  (setq-default
   pcache-directory (concat core-var-cache-dir-full-path "pcache/")
   url-configuration-directory (concat core-var-cache-dir-full-path "url/")
   url-cache-directory (concat core-var-cache-dir-full-path "url/cache"))

  ;; Increase GC limits and remove file handlers for startup
  (defvar core--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold 536870912   ; 512MB
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

  (defun core--finalize-startup ()
    ;; Reset GC limits and file handlers
    (setq gc-cons-threshold 16777216    ; 16MB
          gc-cons-percentage 0.1
          file-name-handler-alist core--file-name-handler-alist))

  (add-hook 'emacs-startup-hook #'core--finalize-startup))

(defun core--get-elisp-compile-dirs ()
  "Get a list of absolute paths of files for byte compilation."
  (cl-loop for dir in (list core-lib-path
                            core-modules-lib-path
                            core-var-lib-path
                            core-user-dir)
           if dir
           collect (expand-file-name dir user-emacs-directory)))

(defun core--init-load-path ()
  "Add required paths to the `load-path' variable."
  (cl-loop for dir in (list core-lib-path
                            core-modules-lib-path
                            core-var-lib-path
                            core-user-dir)
           if dir
           collect dir
           and do (add-to-list
                   'load-path
                   (expand-file-name dir user-emacs-directory))))

(defun core--load-dir (dir)
  "Load all Emacs Lisp files from directory DIR."
  (let* ((files (directory-files dir))
         (file-names (cl-loop for file in files
                              collect (file-name-base file)))
         (packages (cl-remove-duplicates
                    (cl-remove-if
                     (lambda (x)
                       (or (equal x ".")
                           (equal x ".gitignore")
                           (equal x ".gitkeep")))
                     file-names)
                    :test #'equal)))
    (cl-loop for pkg in packages
             collect pkg
             and do (load pkg))))

(defun core--check-and-install-required-packages ()
  "Check and install required packages.
Required packages are defined by `core--required-packages'."
  (cl-loop for pkg in core--required-packages
           do (straight-use-package pkg)))

(defun core:is-windows-p ()
  "Check if the current OS is Windows."
  (equal system-type 'windows-nt))

(defun core:compile-file (file)
  "Compile/recompile an Emacs Lisp file FILE."
  (if (file-exists-p (concat file "c"))
      (byte-recompile-file file)
    (byte-compile-file file)))

(defun core:initialize-packages-and-modules ()
  "Initialize the packages and modules sub-system."

  (defvar bootstrap-version)
  (defvar straight-repository-user "raxod502")
  (defvar straight-repository-branch "develop")
  (defvar straight-base-dir (expand-file-name core-packages-path user-emacs-directory))

  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
        (bootstrap-version 5))
    (unless (or (require 'straight nil t)
                (file-readable-p bootstrap-file))
      (with-current-buffer
          (url-retrieve-synchronously
           (format "https://raw.githubusercontent.com/%s/straight.el/%s/install.el"
                   straight-repository-user
                   straight-repository-branch)
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil t))

  (core--check-and-install-required-packages)
  (core--init-load-path)
  (setq straight-use-package-by-default t
        straight-enable-package-integration nil
        straight-cache-autoloads nil
        ;; Set package.el variables just in case to avoid polluting
        ;; the root directory.
        package-enable-at-startup nil
        package-user-dir (expand-file-name (concat core-packages-path "elpa/")
                                           user-emacs-directory)
        package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir))

  ;; Require only a few packages here and the rest when they're
  ;; needed. They should be available on the `load-path' as
  ;; `package-initialize' has been called.
  (eval-when-compile
    (require 'use-package))
  (require 'bind-key)

  (require 'core-keys)
  (require 'core-extensions)
  (require 'core-customize)
  (require 'core-themes)
  (require 'core-ui)
  ;; core-editor.el is not loaded here as it's not yet needed.
  (core--load-dir (concat user-emacs-directory core-var-lib-path)))

(defun core:load-user-dir ()
  "Load all Emacs Lisp files from directory `core-user-dir'."
  (when core-user-dir
    (core--load-dir core-user-dir)))

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
