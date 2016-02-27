(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/lib/")
(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'config-pkg)

(use-package diminish :ensure t)
(require 'config-common)

(use-package util
  :load-path "lisp/lib/"
  :bind (("<f6>" . match-paren)
         ("C-%" . match-paren)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-' C-y" . yas-global-mode)
         ("C-' y" . yas-global-mode))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/data/snippets/"))

;; (require 'diff-hl)
(require 'edit-server)
(require 'expand-region)
;; (require 'font-utils)
(require 'lacarte)
(require 'load-var-dir)
(require 'multiple-cursors)
(require 'rainbow-delimiters)
(require 'regions)
;; (require 'util)
(require 'yascroll)
;; (require 'yasnippet)

(require 'config-ac)
(require 'config-bookmarks)
(require 'config-c)
(require 'config-clojure)
(require 'config-comment-annotations)
(require 'config-csharp)
(require 'config-cursor)
(require 'config-diff-hl)
(require 'config-dired)
(require 'config-ediff)
(require 'config-elisp)
(require 'config-gnuplot)
(require 'config-god)
(require 'config-gud)
(require 'config-haskell)
(require 'config-helm)
(require 'config-hideshow)
(require 'config-java)
(require 'config-js)
(require 'config-magit)
(require 'config-modeline)
(require 'config-org)
(require 'config-paredit)
(require 'config-php)
(require 'config-python)
(require 'config-ruby)
(require 'config-scala)
(require 'config-scheme)
(require 'config-search)
(require 'config-sql)
(require 'config-sticky)
(require 'config-web)

;; Linux-only config
(unless (is-windows?)
  (require 'config-c)
  (eval-after-load 'info
    '(progn
       (push "/opt/local/share/info" Info-default-directory-list)
       (push "~/.emacs.d/info" Info-default-directory-list)))
  (push "/usr/bin/" exec-path)
  (global-set-key (kbd "C-x ?") 'woman))

;; Windows-only config
(when (is-windows?)
  (setq w32-get-true-file-attributes nil)
  (w32-send-sys-command 61488))

(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x a n"))

(global-set-key (kbd "<apps>") 'execute-extended-command)
;; (global-set-key (kbd "<f6>") 'match-paren)
(global-set-key (kbd "C-! +") 'ediff)
(global-set-key (kbd "C-! =") 'ediff-buffers)
(global-set-key (kbd "C-! C-+") 'ediff)
(global-set-key (kbd "C-! C-=") 'ediff-buffers)
(global-set-key (kbd "C-! C-c") 'calendar)
(global-set-key (kbd "C-! C-e") 'split-and-eshell)
(global-set-key (kbd "C-! C-k") 'compile)
(global-set-key (kbd "C-! C-n") 'calculator)
(global-set-key (kbd "C-! C-p") 'list-processes-and-switch)
;; (global-set-key (kbd "C-%") 'match-paren)
(global-set-key (kbd "C-' ' c") 'camelCase-mode)
(global-set-key (kbd "C-' ' q") 'auto-fill-mode)
(global-set-key (kbd "C-' .") 'highlight-symbol-mode)
(global-set-key (kbd "C-' C-' C-c") 'camelCase-mode)
(global-set-key (kbd "C-' C-' C-q") 'auto-fill-mode)
(global-set-key (kbd "C-' C-.") 'highlight-symbol-mode)
(global-set-key (kbd "C-' C-l") 'hl-line-mode)
(global-set-key (kbd "C-' C-n") 'linum-mode)
(global-set-key (kbd "C-' C-w") 'toggle-truncate-lines)
(global-set-key (kbd "C-' l") 'hl-line-mode)
(global-set-key (kbd "C-' n") 'linum-mode)
(global-set-key (kbd "C-' w") 'toggle-truncate-lines)
(global-set-key (kbd "C-+") 'w-resize)
(global-set-key (kbd "C-: :") 'magit-status)
(global-set-key (kbd "C-: <f10>") 'vc-ediff)
(global-set-key (kbd "C-: C-:") 'magit-status)
(global-set-key (kbd "C-: C-=") 'vc-ediff)
(global-set-key (kbd "C-: C-c C-d") 'git-diff-tree)
(global-set-key (kbd "C-: C-d") 'vc-diff)

(global-set-key (kbd "C-: C-k") 'magit-run-gitk)
(global-set-key (kbd "C-: C-l") 'magit-log-current)
(global-set-key (kbd "C-: C-s") 'magit-status)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<f4>") 'hl-line-mode)
(global-set-key (kbd "C-<f6>") 'linum-mode)
(global-set-key (kbd "C-<f9>") 'toggle-truncate-lines)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-?") 'info-lookup-symbol)
(global-set-key (kbd "C-`") 'sticky-control-mode)
(global-set-key (kbd "C-c (") 'paredit-mode)
(global-set-key (kbd "C-c C-(") 'paredit-mode)
(global-set-key (kbd "C-c C-<f3>") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-\\") 'just-one-space)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c C-n") 'org-agenda)
(global-set-key (kbd "C-c \\") 'just-one-space)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c n") 'org-agenda)

(global-set-key (kbd "C-x '") 'switch-to-scratch)
(global-set-key (kbd "C-x 9") 'delete-single-window)
(global-set-key (kbd "C-x <C-M-return>") 'find-user-init-file)
(global-set-key (kbd "C-x <C-return>") 'mc/edit-lines)
(global-set-key (kbd "C-x <f10>") 'ediff-buffers)
(global-set-key (kbd "C-x <f11>") 'calendar)
(global-set-key (kbd "C-x <f12>") 'calculator)
(global-set-key (kbd "C-x <f3>") 'list-processes-and-switch)
(global-set-key (kbd "C-x <f5>") 'compile)
(global-set-key (kbd "C-x <f7>") 'split-and-eshell)
(global-set-key (kbd "C-x ?") 'woman)
(global-set-key (kbd "C-x C-'") 'switch-to-scratch-other-window)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-5 C-0") 'delete-frame)
(global-set-key (kbd "C-x C-5 C-1") 'delete-other-frames)
(global-set-key (kbd "C-x C-5 C-2") 'make-frame-command)
(global-set-key (kbd "C-x C-9") 'delete-single-window)
(global-set-key (kbd "C-x C-a C-k") 'recompile)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'confirm-and-kill-terminal)

(global-set-key (kbd "C-x M-[") 'previous-buffer)
(global-set-key (kbd "C-x M-]") 'next-buffer)
(global-set-key (kbd "C-x RET RET") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-x S-<f10>") 'ediff)
(global-set-key (kbd "C-x \"") 'switch-to-scratch-other-frame)
(global-set-key (kbd "C-x a k") 'recompile)

(global-set-key (kbd "C-x w") 'save-buffer)
(global-set-key (kbd "C-x |") 'find-user-init-file)
(global-set-key (kbd "C-|") 'move-to-window)
(global-set-key (kbd "ESC M-x") 'lacarte-execute-menu-command)
(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-<f5>") 'recompile)
(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-n") 'move-line-region-down)
(global-set-key (kbd "M-p") 'move-line-region-up)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(defconst backup-dir "~/.emacs-saves/")
(defconst config-custom-font "Courier Prime Code")

(custom-set-variables
 ;;; vars set before loading theme
 '(solarized-use-variable-pitch) nil
 '(solarized-scale-org-headlines nil)
 '(solarized-distinct-fringe-background t)
 '(x-underline-at-descent-line t)
 ;;; add any vars to set here
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-save-file-name-transforms `((".*" ,backup-dir t)))
 '(auto-save-list-file-prefix backup-dir)
 '(backup-directory-alist `((".*" . ,backup-dir)))
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(echo-keystrokes 0.05)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fancy-splash-image nil)
 '(hl-sexp-background-colors (quote ("gray27" "midnight blue")))
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-auto-indent-flag t)
 '(js-indent-level 2)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/TODO.org"
     "~/Dropbox/org/code.org"
     "~/Dropbox/org/birthdays.org"
     "~/Dropbox/org/M-clj.org"
     )))
 '(rainbow-delimiters-highlight-braces-p nil)
 '(rainbow-delimiters-highlight-brackets-p nil)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#00ff00"))

(custom-set-faces
 '(default ((t (:foundry "monotype" :slant normal :weight normal :height 130 :width normal))))
 '(button ((t (:background "green" :foreground "black"))))
 '(cursor ((t (:background "green"))))
 '(escape-glyph ((t (:foreground "#ddaa6f" :weight bold))))
 '(header-line ((((class color) (min-colors 89)) (:background "#303030" :foreground "#e7f6da"))))
 '(helm-ff-directory ((t (:background "LightGray" :foreground "black"))))
 '(helm-swoop-target-word-face ((t (:foreground "green"))))
 '(highlight ((t (:background "#454545" :foreground "#ffffff"))))
 '(highlight-symbol-face ((t (:foreground "green"))))
 '(hl-line ((t (:background "gray27" :foreground "green"))))
 '(isearch ((t (:background "green" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "#384048" :foreground "#a0a8b0"))))
 '(minibuffer-prompt ((t (:foreground "green"))))
 '(mode-line ((t (:background "green1" :foreground "black"))))
 '(mode-line-buffer-id ((t (:background "green" :foreground "black" :weight bold))))
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
 '(yascroll:thumb-fringe ((t (:background "lawn green" :foreground "lawn green")))))

;; custom font
(use-package font-utils
  :load-path "lisp/lib/"
  :init
  (require 'font-utils)
  (progn
    (when (font-utils-exists-p config-custom-font)
      (set-frame-font config-custom-font nil t))))

(use-package paren
  :config
  (set-face-background 'show-paren-match "Dodgerblue1")
  (set-face-foreground 'show-paren-match "white")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (show-paren-mode 1))

(use-package util/save
  :bind (("C-s" . save-buffer)
         ("C-x C-s" . isearch-forward))
  :init
  (global-unset-key (kbd "C-s"))
  (global-unset-key (kbd "C-x C-s"))
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;; TODO
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)

;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/data/snippets/")
(autoload 'camelCase-mode "camelCase-mode" nil t)
(lvd-load-dir "~/.emacs.d/lisp/var/")
(set-mode-line-format)
(kill-line-utils-init)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; server processes
(server-start)
(edit-server-start)

;;; disable these modes
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;; enable these modes before loading saved desktop
(blink-cursor-mode 1)
(column-number-mode 1)
;; (show-paren-mode 1)
(recentf-mode 1)
(global-yascroll-bar-mode 1)
(global-auto-complete-mode 1)
;; (global-diff-hl-mode 1)
(desktop-save-mode 1)
;;; enable these modes after loading saved desktop
;; (yas-global-mode 1)

;; comment out this section to disable global god-mode
(set-god-mode "<escape>" "S-<escape>")
;; comment out this section to sticky control key
(set-sticky-mode ?j)
