;;; Packages

(require 'package)

(defun is-windows? ()
  (equal system-type 'windows-nt))

(defun defpkgsource (name-uri-cons)
  (add-to-list 'package-archives name-uri-cons t))

;; Package sources
(defpkgsource '("marmalade" . "https://marmalade-repo.org/packages/"))
(defpkgsource '("melpa" . "https://melpa.milkbox.net/packages/"))

(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; TODO remove these later

(defvar pkg-packages
  '(
    ;; ac-cider
    ;; ac-slime
    ;; ag
    ;; auto-complete
    c-eldoc
    clj-refactor
    clojure-mode
    clojurescript-mode
    csharp-mode
    ;; diff-hl
    ;; dired+
    ;; dired-details
    ;; dired-details+
    ;; direx
    dockerfile-mode
    ;; edit-server
    ;; expand-region
    find-file-in-project
    findr
    geben
    geiser
    ghci-completion
    gist
    gnuplot
    gnuplot-mode
    ;; god-mode
    handlebars-mode
    haskell-mode
    ;; helm
    ;; helm-ls-git
    ;; helm-swoop
    ;; highlight
    ;; highlight-symbol
    ;; idle-highlight-mode
    inflections                         ;; TODO
    ;; inf-ruby
    js2-mode
    js2-refactor
    jump
    ;; lacarte
    ;; magit
    markdown-mode
    ;; multiple-cursors
    ;; omnisharp ;; TODO
    ;; paredit
    php-mode
    popup
    ;; rainbow-delimiters
    ;; rainbow-mode
    restclient
    ;; rinari
    ;; ruby-compilation
    ;; ruby-electric
    ;; ruby-mode
    ;; rvm
    ;; solarized-theme
    ;; scala-mode2
    slime
    slime-js
    ;; smartparens
    smex
    tern
    ;; tern-auto-complete
    ;; web-mode
    yaml-mode
    ;; yari
    ;; yascroll
    ;; yasnippet
    ;; zencoding-mode
    ))

(require 'cl)
(defun pkg-packages-installed-p ()
  (loop for p in pkg-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun pkg-update-packages ()
  (interactive)
  (unless (pkg-packages-installed-p)
    (message "%s" "Refreshing packages...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (p pkg-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

(defmacro defconfig (name &rest body)
  `(defun ,name ()
     (local-set-key "\r" 'newline-and-indent)
     ,@body))

(pkg-update-packages)

;;; end remove

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package diminish :ensure t)

(provide 'config-pkg)
