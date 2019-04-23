;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'core (concat user-emacs-directory "lib/core/core"))

;;; set proxy server, if needed
;;; (setq url-proxy-services '(("http" . "host:port")))
(core:defsource '("org-elpa"  . "https://orgmode.org/elpa/"))
(core:defsource '("melpa"     . "https://melpa.org/packages/"))
(core:initialize-packages-and-modules)

;;; load before language configurations
(require 'mod-company)
(require 'mod-compile)
(require 'mod-editor)
(require 'mod-files)
(require 'mod-gud)
(require 'mod-helm)
(require 'mod-search)
(require 'mod-vc)

;;; language configurations
(require 'mod-lang-c)
(require 'mod-lang-clojure)
(require 'mod-lang-csharp)
(require 'mod-lang-elisp)
(require 'mod-lang-gnuplot)
(require 'mod-lang-go)
(require 'mod-lang-haskell)
(require 'mod-lang-js)
(require 'mod-lang-php)
(require 'mod-lang-powershell)
(require 'mod-lang-python)
(require 'mod-lang-ruby)
(require 'mod-lang-scala)
(require 'mod-lang-scheme)
(require 'mod-lang-sql)

;;; load after language configurations
(require 'mod-docker)
(require 'mod-github)
(require 'mod-org)
(require 'mod-projectile)
(require 'mod-servers)
(require 'mod-web)
