;;; Packages

(require 'package)

(defun pkg/is-windows? ()
  (equal system-type 'windows-nt))

(defun pkg/defsource (name-uri-cons)
  (add-to-list 'package-archives name-uri-cons t))

(defun pkg/initialize-packages ()
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

(provide 'config-pkg)
