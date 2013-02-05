(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;; rainbow parens
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes (quote ("3d84717766913e2deb8cadf7ddee8d64db1a6bcc4de7e4e36fc1bcca8727ded5" "915cfe1e618f69b4eecd8decc182cd97e5b88b675a6d8c54b18fdcc84c4b14a7" "d360b815962bdfb45c4f77dfb9a60d2c9efc72eeb52f96b781201bb271a665b6" "c6174d0904327dc80cf2615340d4ff773e7cb28d295d51b41b0aa78385a5c54a" "cef9254a1332af631898cf793fe3b3081685088db57001899c6ab3148f4cdc85" "79757ce4abcf1d7bbc7698215a7c4a73f0c4a35118a8f9106ebb1bbcf6d5a693" "af07043eef48c8b9868add839d195e78dc21ba7d63a9fd1be3db47ec3cecb860" "446ea630982d51dd3ee7bbb813151a72f2246b2646a2198564b9b8106de8f155" "7641335320fa8e6a311491f58f0e5c3733b44a7936148d4349381d119f6cca83" "946efabf968fa25ced521351c5901a378cad402669cbce92b72983cd852cd750" default)))
 '(ebnf-non-terminal-font (quote (11 Matrix "Black" "White")))
 '(ebnf-terminal-font (quote (11 Matrix "Black" "White")))
 '(eclim-eclipse-dirs (quote ("/usr/lib/eclipse")))
 '(eclimd-executable nil)
 '(ensime-graphical-tooltips t)
 '(ensime-sbt-compile-on-save nil)
 '(fancy-splash-image nil)
 '(global-eclim-mode t)
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/pymatter/")
 '(js-auto-indent-flag t)
 '(js-indent-level 2)
 '(rainbow-delimiters-highlight-braces-p nil)
 '(rainbow-delimiters-highlight-brackets-p nil)
 '(show-paren-mode t)
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#00ff00")
 '(tool-bar-mode nil)

 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(cursor ((t (:background "green"))))
  '(default ((t (:background "black" :foreground "green" :family "Matrix" :foundry "monotype" :slant normal :weight normal :height 113 :width normal))))
  '(button ((t (:background "green" :foreground "black"))))
  '(escape-glyph ((t (:foreground "#ddaa6f" :weight bold))))
  '(header-line ((((class color) (min-colors 89)) (:background "#303030" :foreground "#e7f6da"))))
  '(highlight ((t (:background "#454545" :foreground "#ffffff" :underline t))))
  '(isearch ((t (:background "green" :foreground "black"))))
  '(lazy-highlight ((((class color) (min-colors 89)) (:background "#384048" :foreground "#a0a8b0"))))
  '(minibuffer-prompt ((t (:foreground "green"))))
  '(mode-line ((t (:background "green1" :foreground "black"))))
  '(mode-line-buffer-id ((t (:background "green" :foreground "black" :weight bold :height 0.9))))
  '(mode-line-inactive ((t (:background "black" :foreground "chartreuse"))))
  '(rainbow-delimiters-depth-1-face ((t (:foreground "light sky blue"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "royal blue"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "medium orchid"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "dark orange"))))
  '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
  '(rainbow-delimiters-depth-6-face ((t (:foreground "lawn green"))))
  '(rainbow-delimiters-depth-7-face ((t (:foreground "light sky blue"))))
  '(rainbow-delimiters-depth-8-face ((t (:foreground "royal blue"))))
  '(rainbow-delimiters-depth-9-face ((t (:foreground "royal blue"))))
  '(rainbow-delimiters-unmatched-face ((t (:foreground "medium orchid"))))
  '(region ((t (:background "white" :foreground "black"))))))

(push "/pi/pymatter/scala/bin/" exec-path)
(push "/home/darth10/bin/" exec-path)
(push "/usr/bin/" exec-path)

;; backup settings
(setq backup-directory-alist `(("." . "~/.emacs-saves")))

(require 'yasnippet-bundle)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-delay 0.5) ;; eclipse uses 500ms
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

;; scala-mode
(add-to-list 'load-path "~/.emacs.d/scala-emacs")
(require 'scala-mode-auto)

(add-hook 'scala-mode-hook
	'(lambda ()
		(scala-mode-feature-electric-mode)))

;; nrepl
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; ritz
(setq clojure-swank-command
      (if (or (locate-file "lein" exec-path) (locate-file "lein.bat" exec-path))
	  "lein ritz-in %s"
	"echo \"lein ritz-in %s\" | $SHELL -l"))

;; eclim
(add-to-list 'load-path (expand-file-name "~/.emacs.d/eclim/"))
;; only add the vendor path when you want to use the libraries provided with emacs-eclim
(add-to-list 'load-path (expand-file-name "~/.emacs.d/eclim/vendor"))
(require 'eclim)

(setq eclim-auto-save t)
(global-eclim-mode)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook (lambda ()
                             (add-to-list 'ac-sources 'ac-source-emacs-eclim)
                             (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))

;; c-eldoc
(setq c-eldoc-includes "`pkg-config glib-2.0 gio-2.0 --cflags` -I/usr/include -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; elisp eldoc
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	     (turn-on-eldoc-mode)))

;; info docs
(eval-after-load 'info
  '(progn
     (push "/opt/local/share/info" Info-default-directory-list)
     (push "~/.emacs.d/info" Info-default-directory-list)))

;; org-mode shortcuts
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; my shortcuts

(defun split-and-term ()
  "Split window and start terminal"
  (interactive)
  (split-window-below -10)
  (other-window 1)
  (term "/bin/bash"))

(defun split-and-nrepl-jack-in ()
  "Split window and start nREPL"
  (interactive)
  (split-window-right)
  (nrepl-jack-in nil))

(defun split-and-nrepl ()
  "Split window and start nREPL client"
  (interactive)
  (split-window-right)
  (nrepl "localhost"
	 (string-to-number (read-from-minibuffer "Port: "))))

(defun git-diff-tree ()
  (interactive)
  (magit-diff-working-tree "HEAD"))

(setq gdb-many-windows t)

(global-set-key (kbd "C-x <f7>") 'split-and-term)
(global-set-key (kbd "C-x <f8>") 'split-and-nrepl)
(global-set-key (kbd "C-x C-<f8>") 'split-and-nrepl-jack-in)

; gdb and info shortcuts
(global-set-key (kbd "C-x <f11>") 'gdb)
(global-set-key (kbd "C-x <f3>") 'info-lookup-symbol)
(global-set-key (kbd "C-<f11>") 'gdb-display-gdb-buffer)
(global-set-key (kbd "<f12>") 'gdb-display-source-buffer)
(global-set-key (kbd "<f11>") 'gud-run)
(global-set-key (kbd "<f5>") 'gud-step)
(global-set-key (kbd "<f6>") 'gud-next)
(global-set-key (kbd "<f7>") 'gud-finish)
(global-set-key (kbd "<f8>") 'gud-cont)

(global-set-key (kbd "C-c (") 'paredit-mode)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g d") 'git-diff-tree)

(put 'upcase-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
