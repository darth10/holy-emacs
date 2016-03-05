(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/lib/")
(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'config-pkg)

(use-package diminish :ensure t)
(require 'config-common)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-' C-y" . yas-global-mode)
         ("C-' y" . yas-global-mode))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/data/snippets/"))

(use-package lacarte
  :ensure t
  :bind ("ESC M-x" . lacarte-execute-menu-command))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<f3>" . mc/mark-all-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-x <C-return>" . mc/edit-lines)))

(require 'regions)

(require 'config-lisps)
(require 'config-bookmarks)
(require 'config-c)
(require 'config-clojure)
(require 'config-comment-annotations)
(require 'config-company)
(require 'config-csharp)
(require 'config-cursor)
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
(require 'config-php)
(require 'config-python)
(require 'config-ruby)
(require 'config-scala)
(require 'config-scheme)
(require 'config-search)
(require 'config-smartparens)
(require 'config-sql)
(require 'config-sticky)
(require 'config-web)
(require 'config-ui)

;; Linux-only config
(unless (is-windows?)
  (require 'config-c)
  (eval-after-load 'info
    '(progn
       (push "/opt/local/share/info" Info-default-directory-list)
       (push "~/.emacs.d/info" Info-default-directory-list)))
  (push "/usr/bin/" exec-path))

(use-package woman
  :unless (is-windows?)
  :bind ("C-x ?" . woman))

;; Windows-only config
(when (is-windows?)
  (setq w32-get-true-file-attributes nil)
  (w32-send-sys-command 61488))

(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x a n"))

(global-set-key (kbd "<apps>") 'execute-extended-command)
(global-set-key (kbd "C-! +") 'ediff)
(global-set-key (kbd "C-! =") 'ediff-buffers)
(global-set-key (kbd "C-! C-+") 'ediff)
(global-set-key (kbd "C-! C-=") 'ediff-buffers)
(global-set-key (kbd "C-! C-c") 'calendar)
(global-set-key (kbd "C-! C-e") 'split-and-eshell)
(global-set-key (kbd "C-! C-k") 'compile)
(global-set-key (kbd "C-! C-n") 'calculator)
(global-set-key (kbd "C-! C-p") 'list-processes-and-switch)
(global-set-key (kbd "C-' ' c") 'camelCase-mode)
(global-set-key (kbd "C-' ' q") 'auto-fill-mode)
(global-set-key (kbd "C-' C-' C-c") 'camelCase-mode)
(global-set-key (kbd "C-' C-' C-q") 'auto-fill-mode)
(global-set-key (kbd "C-' C-n") 'linum-mode)
(global-set-key (kbd "C-' C-w") 'toggle-truncate-lines)
(global-set-key (kbd "C-' n") 'linum-mode)
(global-set-key (kbd "C-' w") 'toggle-truncate-lines)
(global-set-key (kbd "C-+") 'w-resize)
(global-set-key (kbd "C-: <f10>") 'vc-ediff)
(global-set-key (kbd "C-: C-=") 'vc-ediff)

(global-set-key (kbd "C-<f6>") 'linum-mode)
(global-set-key (kbd "C-<f9>") 'toggle-truncate-lines)

(global-set-key (kbd "C-`") 'sticky-control-mode)
(global-set-key (kbd "C-c C-\\") 'just-one-space)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c C-n") 'org-agenda)
(global-set-key (kbd "C-c \\") 'just-one-space)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c n") 'org-agenda)

(global-set-key (kbd "C-x '") 'switch-to-scratch)
(global-set-key (kbd "C-x 9") 'delete-single-window)
(global-set-key (kbd "C-x <C-M-return>") 'find-user-init-file)
(global-set-key (kbd "C-x <f10>") 'ediff-buffers)
(global-set-key (kbd "C-x <f11>") 'calendar)
(global-set-key (kbd "C-x <f12>") 'calculator)
(global-set-key (kbd "C-x <f3>") 'list-processes-and-switch)
(global-set-key (kbd "C-x <f5>") 'compile)
(global-set-key (kbd "C-x <f7>") 'split-and-eshell)
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
(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-<f5>") 'recompile)
(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-n") 'move-line-region-down)
(global-set-key (kbd "M-p") 'move-line-region-up)

(defconst backup-dir "~/.emacs-saves/")
(defconst config-custom-font "Courier Prime Code")

(custom-set-variables
 '(auto-save-file-name-transforms `((".*" ,backup-dir t)))
 '(auto-save-list-file-prefix backup-dir)
 '(backup-directory-alist `((".*" . ,backup-dir)))
 '(create-lockfiles nil)
 '(echo-keystrokes 0.05)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(js-auto-indent-flag t)
 '(js-indent-level 2)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/TODO.org"
     "~/Dropbox/org/code.org"
     "~/Dropbox/org/birthdays.org"
     "~/Dropbox/org/M-clj.org"))))

;; custom font
(use-package font-utils
  :load-path "lisp/lib/"
  :config
  (progn
    (when (font-utils-exists-p config-custom-font)
      (set-frame-font config-custom-font nil t))))

(autoload 'camelCase-mode "camelCase-mode" nil t)
(set-mode-line-format)

;;; server process
(use-package server
  :ensure t
  :config
  (server-start))

;;; edit-server process
(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

;;; enable these modes before loading saved desktop
(blink-cursor-mode 1)
(column-number-mode 1)
(recentf-mode 1)
(desktop-save-mode 1)

(use-package util
  :load-path "lisp/lib/"
  :bind (("<f6>" . match-paren)
         ("C-%" . match-paren))
  :init
  ;; isearch-mode-map
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)
  ;; enable disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :config
  (kill-line-utils-init)
  (lvd-load-dir "~/.emacs.d/lisp/var/"))

(use-package util/save
  :bind (("C-s" . save-buffer)
         ("C-x C-s" . isearch-forward))
  :init
  (global-unset-key (kbd "C-s"))
  (global-unset-key (kbd "C-x C-s"))
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; comment out this section to disable global god-mode
(set-god-mode "<escape>" "S-<escape>")
;; comment out this section to sticky control key
(set-sticky-mode ?j)
