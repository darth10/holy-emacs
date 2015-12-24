(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/lib/")
(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'config-common)
(toggle-uniquify-buffer-names)

;; check for packages to install
(require 'config-pkg)
(pkg-update-packages)

(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x a n"))

;; god-mode
;; comment out this section to disable global god-mode
(require 'config-god)
(set-god-mode "<escape>" "S-<escape>")

(global-set-key (kbd "C-x w") 'save-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-x C-s") 'isearch-forward)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-9") 'delete-single-window)
(global-set-key (kbd "C-x 9") 'delete-single-window)
(global-set-key (kbd "C-x C-5 C-0") 'delete-frame)
(global-set-key (kbd "C-x C-5 C-1") 'delete-other-frames)
(global-set-key (kbd "C-x C-5 C-2") 'make-frame-command)

(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)

;; comment out this section to sticky control key
(require 'config-sticky)
(global-set-key (kbd "C-`") 'sticky-control-mode)
(set-sticky-mode ?j)

;; highlight current line
(add-hook 'prog-mode-hook 'hl-line-mode)

;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(custom-set-variables
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote ("3d84717766913e2deb8cadf7ddee8d64db1a6bcc4de7e4e36fc1bcca8727ded5"
           "915cfe1e618f69b4eecd8decc182cd97e5b88b675a6d8c54b18fdcc84c4b14a7"
           "d360b815962bdfb45c4f77dfb9a60d2c9efc72eeb52f96b781201bb271a665b6"
           "c6174d0904327dc80cf2615340d4ff773e7cb28d295d51b41b0aa78385a5c54a"
           "cef9254a1332af631898cf793fe3b3081685088db57001899c6ab3148f4cdc85"
           "79757ce4abcf1d7bbc7698215a7c4a73f0c4a35118a8f9106ebb1bbcf6d5a693"
           "af07043eef48c8b9868add839d195e78dc21ba7d63a9fd1be3db47ec3cecb860"
           "446ea630982d51dd3ee7bbb813151a72f2246b2646a2198564b9b8106de8f155"
           "7641335320fa8e6a311491f58f0e5c3733b44a7936148d4349381d119f6cca83"
           "946efabf968fa25ced521351c5901a378cad402669cbce92b72983cd852cd750"
           default)))
 '(fancy-splash-image nil)
 '(hl-sexp-background-colors (quote ("gray27" "midnight blue")))
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(echo-keystrokes 0.05)
 '(js-auto-indent-flag t)
 '(js-indent-level 2)
 '(rainbow-delimiters-highlight-braces-p nil)
 '(rainbow-delimiters-highlight-brackets-p nil)
 '(show-paren-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#00ff00")
 '(tool-bar-mode nil))

(custom-set-faces
 '(cursor ((t (:background "green"))))
 '(default ((t (:background "black" :foreground "green"
                            :foundry "monotype" :slant normal :weight normal
                            :height 130 :width normal))))
 '(button ((t (:background "green" :foreground "black"))))
 '(escape-glyph ((t (:foreground "#ddaa6f" :weight bold))))
 '(header-line ((((class color) (min-colors 89))
                 (:background "#303030" :foreground "#e7f6da"))))
 '(helm-ff-directory ((t (:background "LightGray" :foreground "black"))))
 '(helm-header ((t (:background "black" :foreground "dim gray"))))
 '(helm-separator ((t (:foreground "gray"))))
 '(helm-source-header
   ((t (:background "green" :foreground "black"
                    :weight bold :height 1.0 :family "Courier New"))))
 '(highlight ((t (:background "#454545" :foreground "#ffffff"))))
 '(hl-line ((t (:background "gray27" :foreground "green"))))
 '(isearch ((t (:background "green" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 89))
                    (:background "#384048" :foreground "#a0a8b0"))))
 '(minibuffer-prompt ((t (:foreground "green"))))
 '(mode-line ((t (:background "green1" :foreground "black"))))
 '(mode-line-buffer-id ((t (:background "green" :foreground "black"
                                        :weight bold :height 0.9))))
 '(mode-line-inactive ((t (:background "dimgray" :foreground "black"))))
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
 '(yascroll:thumb-fringe
   ((t (:background "lawn green" :foreground "lawn green")))))

;; custom font
(require 'font-utils)
(defconst config-custom-font "Matrix")
(progn
  (when (font-utils-exists-p config-custom-font)
    (set-frame-font config-custom-font nil t)))

(unless (is-windows?)
  (push "~/pymatter/bin/" exec-path)
  (push "/usr/bin/" exec-path))

;; move regions
(require 'regions)
(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-p") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-n") 'move-line-region-down)

;; backup settings
(defconst backup-dir "~/.emacs-saves/")
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
(setq auto-save-list-file-prefix backup-dir)

;; yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/data/snippets/")
(yas-global-mode 1)

;; mode configurations
(require 'config-paredit)
(require 'config-helm)
(require 'config-magit)
(require 'config-ediff)
(require 'config-dired)
(require 'config-org)
(require 'config-web)
(require 'config-bookmarks)
(require 'config-ac)
(require 'config-comment-annotations)
(require 'config-cursor)
(require 'config-modeline)
(require 'config-hideshow)

;; language configurations
(require 'config-js)
(require 'config-clojure)
(require 'config-csharp)
(require 'config-elisp)
(require 'config-scheme)
(require 'config-scala)
(require 'config-haskell)
(require 'config-php)
(require 'config-python)
(require 'config-ruby)
(require 'config-sql)
(require 'config-java)
(require 'config-gnuplot)
(require 'config-gud)
(require 'config-search)

;; linux-only languages
(unless (is-windows?)
  (require 'config-c))

;; info docs
(unless (is-windows?)
  (eval-after-load 'info
    '(progn
       (push "/opt/local/share/info" Info-default-directory-list)
       (push "~/.emacs.d/info" Info-default-directory-list))))

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x <C-return>") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<f3>") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

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
(global-set-key (kbd "C-x r =") 'interactive-diff-hl-update)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'util)

;; linux-only key bindings
(unless (is-windows?)
  (global-set-key (kbd "C-x ?") 'woman))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'confirm-and-kill-terminal)

(global-set-key (kbd "<apps>") 'execute-extended-command)
(global-set-key (kbd "C-x <f7>") 'split-and-eshell)
(global-set-key (kbd "C-! C-e") 'split-and-eshell)
(global-set-key (kbd "C-x <f3>") 'list-processes-and-switch)
(global-set-key (kbd "C-! C-p") 'list-processes-and-switch)
(global-set-key (kbd "C-x <f10>") 'ediff-buffers)
(global-set-key (kbd "C-! C-=") 'ediff-buffers)
(global-set-key (kbd "C-! =") 'ediff-buffers)
(global-set-key (kbd "C-x S-<f10>") 'ediff)
(global-set-key (kbd "C-! C-+") 'ediff)
(global-set-key (kbd "C-! +") 'ediff)
(global-set-key (kbd "C-' C-' C-q") 'auto-fill-mode)
(global-set-key (kbd "C-' ' q") 'auto-fill-mode)

(global-set-key (kbd "C-x <f11>") 'calendar)
(global-set-key (kbd "C-! C-c") 'calendar)
(global-set-key (kbd "C-x <f12>") 'calculator)
(global-set-key (kbd "C-! C-n") 'calculator)
(global-set-key (kbd "C-x <C-M-return>") 'find-user-init-file)
(global-set-key (kbd "C-x |") 'find-user-init-file)
(global-set-key (kbd "C-x RET RET") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-x M-[") 'previous-buffer)
(global-set-key (kbd "C-x M-]") 'next-buffer)
(global-set-key (kbd "C-<f4>") 'hl-line-mode)
(global-set-key (kbd "C-' C-l") 'hl-line-mode)
(global-set-key (kbd "C-' l") 'hl-line-mode)
(global-set-key (kbd "C-<f6>") 'linum-mode)
(global-set-key (kbd "C-' C-n") 'linum-mode)
(global-set-key (kbd "C-' n") 'linum-mode)
(global-set-key (kbd "C-<f9>") 'toggle-truncate-lines)
(global-set-key (kbd "C-' C-w") 'toggle-truncate-lines)
(global-set-key (kbd "C-' w") 'toggle-truncate-lines)
(global-set-key (kbd "C-+") 'w-resize)
(global-set-key (kbd "C-|") 'move-to-window)
(global-set-key (kbd "C-?") 'info-lookup-symbol)
(global-set-key (kbd "C-c (") 'paredit-mode)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c C-\\") 'just-one-space)
(global-set-key (kbd "C-c \\") 'just-one-space)
(global-set-key (kbd "<f6>") 'match-paren)
(global-set-key (kbd "C-%") 'match-paren)

(global-set-key (kbd "C-x \"") 'switch-to-scratch-other-frame)
(global-set-key (kbd "C-x '") 'switch-to-scratch)
(global-set-key (kbd "C-x C-'") 'switch-to-scratch-other-window)

;;; Magit and vc

(global-set-key (kbd "C-: C-s") 'magit-status)
(global-set-key (kbd "C-: C-:") 'magit-status)
(global-set-key (kbd "C-: :") 'magit-status)
(global-set-key (kbd "C-: <f10>") 'vc-ediff)
(global-set-key (kbd "C-: C-=") 'vc-ediff)
(global-set-key (kbd "C-: C-d") 'vc-diff)
(global-set-key (kbd "C-: C-c C-d") 'git-diff-tree)
(global-set-key (kbd "C-: C-k") 'magit-run-gitk)
(global-set-key (kbd "C-: C-f") 'helm-git-files)
(global-set-key (kbd "C-: C-l") 'magit-log)

(global-set-key (kbd "C-x <f5>") 'compile)
(global-set-key (kbd "C-! C-k") 'compile)
(global-set-key (kbd "M-<f5>") 'recompile)
(global-set-key (kbd "C-x C-a C-k") 'recompile)
(global-set-key (kbd "C-x a k") 'recompile)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; camelCase mode
(autoload 'camelCase-mode "camelCase-mode" nil t)
(global-set-key (kbd "C-' C-' C-c") 'camelCase-mode)
(global-set-key (kbd "C-' ' c") 'camelCase-mode)

;; Windows configuration
(when (is-windows?)
  (setq w32-get-true-file-attributes nil)
  (w32-send-sys-command 61488))

(server-start)

;; edit server

(require 'edit-server)
(edit-server-start)

;; workgroups
;; make sure this is started last for performance
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-x C-:"))
(setq wg-default-session-file "~/.emacs_workgroups")
(setq wg-first-wg-name "default")
(workgroups-mode 1)

(require 'load-var-dir)
(lvd-load-dir "~/.emacs.d/lisp/var/")
