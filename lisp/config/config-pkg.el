;;; Packages

(require 'cl)
(require 'package)

(defvar pkg-packages
  '(ac-nrepl
    auto-complete
    c-eldoc
    clojure-mode
    clojurescript-mode
    clojure-test-mode
    csharp-mode
    edit-server
    find-file-in-project
    findr
    fold-dwim
    fold-dwim-org
    geiser
    ghci-completion
    gist
    haskell-mode
    helm
    helm-git
    helm-ls-git
    highlight
    idle-highlight-mode
    ido-ubiquitous
    inflections
    inf-ruby
    js-comint
    jump
    magit
    markdown-mode
    multiple-cursors
    multi-web-mode
    nrepl
    omnisharp
    paredit
    popup
    rainbow-delimiters
    rainbow-mode
    rinari
    ruby-compilation
    ruby-electric
    ruby-mode
    rvm
    scala-mode2
    smex
    yaml-mode
    yari))

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
