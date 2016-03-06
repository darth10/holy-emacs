(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'config-pkg)

;;; TODO
(require 'config-common)

;;; load before language configurations
(require 'config-company)
(require 'config-gud)
(require 'config-helm)
(require 'config-utils)
(require 'config-vc)

;;; language configurations
(require 'config-c)
(require 'config-clojure)
(require 'config-csharp)
(require 'config-elisp)
(require 'config-gnuplot)
(require 'config-haskell)
(require 'config-java)
(require 'config-js)
(require 'config-org)
(require 'config-php)
(require 'config-python)
(require 'config-ruby)
(require 'config-scala)
(require 'config-scheme)
(require 'config-sql)

;;; load after language configurations
(require 'config-lisps)
(require 'config-modes)
(require 'config-servers)
(require 'config-ui)
(require 'config-web)

;; Linux-only config
(unless (is-windows?)
  (require 'config-c)
  (eval-after-load 'info
    '(progn
       (push "/opt/local/share/info" Info-default-directory-list)
       (push "~/.emacs.d/info" Info-default-directory-list)))
  (push "/usr/bin/" exec-path))

;; Windows-only config
(when (is-windows?)
  (setq w32-get-true-file-attributes nil)
  (w32-send-sys-command 61488))

(desktop-save-mode 1)

(modes/set-mode-line-format)
;; ;; comment out this line to disable god-mode
(modes/set-god-mode "<escape>" "S-<escape>")
;; comment out this line to disable sticky-control-mode
(modes/set-sticky-control-mode t)
