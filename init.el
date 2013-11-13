(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/lib/")
(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'config-common)

;; check for packages to install
(require 'config-pkg)
(pkg-update-packages)

;; highlight current line
(global-hl-line-mode t)

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
 '(fancy-splash-image nil)
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
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
  '(helm-ff-directory ((t (:background "LightGray" :foreground "black"))))
  '(helm-header ((t (:background "black" :foreground "dim gray"))))
  '(helm-separator ((t (:foreground "gray"))))
  '(helm-source-header ((t (:background "green" :foreground "black" :weight bold :height 1.0 :family "Courier New"))))
  '(highlight ((t (:background "#454545" :foreground "#ffffff"))))
  '(hl-line ((t (:background "gray27" :foreground "green"))))
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
  '(region ((t (:background "white" :foreground "black"))))
  '(yascroll:thumb-fringe ((t (:background "lawn green" :foreground "lawn green"))))))

(unless (is-windows?)
  (push "~/pymatter/bin/" exec-path)
  (push "/usr/bin/" exec-path))

;; move regions
(require 'regions)
(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)

;; backup settings
(defconst backup-dir "~/.emacs-saves/")
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
(setq auto-save-list-file-prefix backup-dir)

;; yasnippet
(require 'yasnippet-bundle)
(setq yas/root-directory "~/.emacs.d/data/snippets")
(yas/load-directory yas/root-directory)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/data/ac-dict")
(ac-config-default)
(setq ac-delay 0.5) ;; eclipse uses 500ms
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

;; code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'fold-dwim-org/minor-mode)

;; mode configurations
(require 'config-paredit)
(require 'config-helm)
(require 'config-ediff)
(require 'config-dired)
(require 'config-org)
(require 'config-web)
(require 'config-comment-annotations)

;; language configurations
(require 'config-js)
(require 'config-clojure)
(require 'config-csharp)
(require 'config-elisp)
(require 'config-scheme)
(require 'config-scala)
(require 'config-haskell)
(require 'config-python)
(require 'config-ruby)
(require 'config-sql)

;; linux-only languages
(unless (is-windows?)
  (require 'config-c))

;; info docs
(unless (is-windows?)
  (eval-after-load 'info
    '(progn
       (push "/opt/local/share/info" Info-default-directory-list)
       (push "~/.emacs.d/info" Info-default-directory-list))))

(global-set-key (kbd "C-c C-=") 'bc-set)
(global-set-key (kbd "C-c C-;") 'bc-list)
(global-set-key (kbd "C-c C-#") 'bc-clear-and-msg)
(global-set-key (kbd "C-c C-<left>") 'bc-previous)
(global-set-key (kbd "C-c C-<right>") 'bc-next)
(global-set-key (kbd "C-c C-<up>") 'bc-local-previous)
(global-set-key (kbd "C-c C-<down>") 'bc-local-next)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x <C-return>") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<f3>") 'mc/mark-all-like-this)

;; dynamic cursor color
(require 'hcz-cursor)
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

;; lacarte menu
(require 'lacarte)
(menu-bar-mode -1)
(global-set-key (kbd "ESC M-x") 'lacarte-execute-menu-command)

;; yascroll
(require 'yascroll)
(toggle-scroll-bar -1)
(global-yascroll-bar-mode t)

;; diff-hl
(require 'diff-hl)
(global-diff-hl-mode t)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'util)

;; linux-only key bindings
(unless (is-windows?)
  (global-set-key (kbd "C-x ?") 'woman))

(global-set-key (kbd "C-x <f7>") 'split-and-eshell)
(global-set-key (kbd "C-x <f3>") 'list-processes-and-switch)
(global-set-key (kbd "C-x <f10>") 'ediff-buffers)
(global-set-key (kbd "C-x S-<f10>") 'ediff)
(global-set-key (kbd "C-x <f11>") 'calendar)
(global-set-key (kbd "C-x <f12>") 'calculator)
(global-set-key (kbd "C-x <C-M-return>") 'find-user-init-file)
(global-set-key (kbd "C-x RET RET") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-x G") 'grep)
(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key (kbd "C-<f2>") 'helm-imenu)
(global-set-key (kbd "C-<f4>") 'global-hl-line-mode)
(global-set-key (kbd "C-<f6>") 'linum-mode)
(global-set-key (kbd "C-<f9>") 'toggle-truncate-lines)
(global-set-key (kbd "C-+") 'w-resize)
(global-set-key (kbd "C-|") 'move-to-window)
(global-set-key (kbd "C-?") 'info-lookup-symbol)
(global-set-key (kbd "C-c (") 'paredit-mode)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-<XF86Back>") 'previous-buffer)
(global-set-key (kbd "C-<XF86Forward>") 'next-buffer)
(global-set-key (kbd "<f11>") 'match-paren)

(global-set-key (kbd "M-g M-s") 'magit-status)
(global-set-key (kbd "M-g <f3>") 'vc-git-grep)
(global-set-key (kbd "M-g <f10>") 'vc-ediff)
(global-set-key (kbd "M-g d") 'vc-diff)
(global-set-key (kbd "M-g M-d") 'git-diff-tree)
(global-set-key (kbd "M-g M-f") 'helm-ls-git-ls)
(global-set-key (kbd "M-g M-l") 'magit-log)

(global-set-key (kbd "C-x <f5>") 'compile)
(global-set-key (kbd "M-<f5>") 'recompile)

(require 'highlight-token)
(global-set-key (kbd "M-]") 'hlt-highlight-current-word)
(global-set-key (kbd "ESC M-]") 'hlt-unhighlight-all-prop)

;; search
(require 'ag)
(global-set-key (kbd "C-<f3>") 'ag)
(global-set-key (kbd "C-S-<f3>") 'ag-regexp)
(global-set-key (kbd "C-c <f3>") 'ag-project)
(global-set-key (kbd "C-c S-<f3>") 'ag-project-regexp)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Windows configuration
(when (is-windows?)
  (setq w32-get-true-file-attributes nil)
  (w32-send-sys-command 61488))


(server-start)

;; edit server

(require 'edit-server)
(edit-server-start)
