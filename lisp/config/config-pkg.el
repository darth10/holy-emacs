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
    haskell-mode
    helm
    helm-git-files
    highlight
    idle-highlight-mode
    inflections
    inf-ruby
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
    rinari
    ruby-compilation
    ruby-electric
    ruby-mode
    rvm
    scala-mode2
    slime
    slime-js
    smex
    tern
    web-mode
    workgroups2
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
