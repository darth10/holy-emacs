;;; Packages

(require 'cl)
(require 'package)

(defvar pkg-packages
  '(ac-cider
    ac-slime
    ag
    auto-complete
    c-eldoc
    clj-refactor
    clojure-mode
    clojurescript-mode
    csharp-mode
    diff-hl
    dired+
    dired-details
    dired-details+
    direx
    dockerfile-mode
    edit-server
    expand-region
    find-file-in-project
    findr
    geben
    geiser
    ghci-completion
    gist
    gnuplot
    gnuplot-mode
    god-mode
    handlebars-mode
    haskell-mode
    helm
    helm-ls-git
    helm-swoop
    highlight
    highlight-symbol
    idle-highlight-mode
    inflections
    inf-ruby
    js2-mode
    js2-refactor
    jump
    lacarte
    magit
    markdown-mode
    multiple-cursors
    omnisharp
    paredit
    php-mode
    popup
    rainbow-delimiters
    rainbow-mode
    restclient
    rinari
    ruby-compilation
    ruby-electric
    ruby-mode
    rvm
    solarized-theme
    scala-mode2
    slime
    slime-js
    smartparens
    smex
    tern
    tern-auto-complete
    web-mode
    yaml-mode
    yari
    yascroll
    yasnippet
    zencoding-mode))

(defun defpkgsource (name-uri)
  (add-to-list 'package-archives name-uri t))

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

;; Package sources
(defpkgsource '("marmalade" . "http://marmalade-repo.org/packages/"))
(defpkgsource '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(provide 'config-pkg)
