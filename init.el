(add-to-list 'load-path "~/.emacs.d/lisp/lib/")
(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'core)
;;; set proxy server, if needed
;;; (setq url-proxy-services '(("http" . "host:port")))
(core/defsource '("org-elpa" . "https://orgmode.org/elpa/"))
(core/defsource '("marmalade" . "https://marmalade-repo.org/packages/"))
(core/defsource '("melpa" . "https://melpa.org/packages/"))
(core/initialize-packages)

;;; load before language configurations
(require 'config-company)
(require 'config-gud)
(require 'config-helm)
(require 'config-lisps)
(require 'config-utils)
(require 'config-vc)

;;; language configurations
(require 'config-c)
(require 'config-clojure)
(require 'config-csharp)
(require 'config-elisp)
(require 'config-gnuplot)
(require 'config-go)
(require 'config-haskell)
(require 'config-js)
(require 'config-org)
(require 'config-php)
(require 'config-powershell)
(require 'config-python)
(require 'config-ruby)
(require 'config-scala)
(require 'config-scheme)
(require 'config-sql)

;;; load after language configurations
(require 'config-modes)
(require 'config-servers)
(require 'config-ui)
(require 'config-web)
