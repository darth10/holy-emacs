;;; Configuration for utility packages

(defconst config-custom-font "Courier Prime Code")
(defconst backup-dir "~/.emacs.bak/")

(use-package wide-column
  :ensure t
  :diminish wide-column-mode)

(use-package grep
  :bind (("M-s G" . grep)
         ("M-s g" . rgrep)))

(use-package vc-git
  :bind (("C-: <f3>" . vc-git-grep)
         ("M-s :" . vc-git-grep)
         ("C-: M-s" . vc-git-grep)))

(use-package ag
  :ensure t
  :bind (("C-<f3>" . ag)
         ("M-s s" . ag)
         ("C-S-<f3>" . ag-regexp)
         ("M-s r" . ag-regexp)
         ("M-s a s" . ag-project)
         ("M-s a r" . ag-project-regexp)))

(use-package dired
  :config
  (use-package dired+
    :ensure t)

  (use-package direx
    :bind ("C-c C-j" . direx:jump-to-directory-other-window)
    :ensure t)

  (defun configure-dired ()
    (local-set-key (kbd "C-x C-/") 'wdired-change-to-wdired-mode))

  (bind-key "C-x C-j" 'dired-jump-other-window)
  (add-hook 'dired-mode-hook 'configure-dired))

(use-package hideshow
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-c d" . hs-hide-block)
              ("C-c a d" . hs-hide-all)
              ("C-c C-d" . hs-hide-block)
              ("C-c C-a C-d" . hs-hide-all)
              ("C-c s" . hs-show-block)
              ("C-c a s" . hs-show-all)
              ("C-c C-s" . hs-show-block)
              ("C-c C-a C-s" . hs-show-all))
  :config
  (add-hook 'clojure-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'csharp-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook 'hs-minor-mode)
  (add-hook 'lisp-mode-hook 'hs-minor-mode)
  (add-hook 'sh-mode-hook 'hs-minor-mode))

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

(use-package woman
  :unless (is-windows?)
  :bind ("C-x ?" . woman))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package regions
  :load-path "lisp/lib/"
  :bind (("M-<down>" . move-line-region-down)
         ("M-<up>" . move-line-region-up)
         ("M-n" . move-line-region-down)
         ("M-p" . move-line-region-up)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package camelCase
  :diminish camelCase-mode
  :load-path "lisp/lib/"
  :bind (("C-' c" . camelCase-mode)
         ("C-' C-c" . camelCase-mode)))

(use-package recentf
  :bind (("C-c : ;" . recentf-open-files)
         ("C-c C-: C-;" . recentf-open-files))
  :config
  (custom-set-variables
   '(recentf-max-menu-items 40)
   '(recentf-max-saved-items 40))
  (recentf-mode t))

(use-package breadcrumb
  :load-path "lisp/lib/"
  :bind (("C-c : #" . bc-clear-and-msg)
         ("C-c : :" . bc-list)
         ("C-c : <down>" . bc-local-next)
         ("C-c : <left>" . bc-previous)
         ("C-c : <right>" . bc-next)
         ("C-c : <up>" . bc-local-previous)
         ("C-c : =" . bc-set)
         ("C-c : b" . bc-previous)
         ("C-c : f" . bc-next)
         ("C-c : n" . bc-local-next)
         ("C-c : p" . bc-local-previous)
         ("C-c C-: C-#" . bc-clear-and-msg)
         ("C-c C-: C-:" . bc-list)
         ("C-c C-: C-<down>" . bc-local-next)
         ("C-c C-: C-<left>" . bc-previous)
         ("C-c C-: C-<right>" . bc-next)
         ("C-c C-: C-<up>" . bc-local-previous)
         ("C-c C-: C-=" . bc-set)
         ("C-c C-: C-b" . bc-previous)
         ("C-c C-: C-f" . bc-next)
         ("C-c C-: C-n" . bc-local-next)
         ("C-c C-: C-p" . bc-local-previous))
  :config
  (defun bc-clear-and-msg ()
    (interactive)
    (bc-clear)
    (message "All breadcrumbs deleted!")))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package calendar
  :bind (("C-! c" . calendar)
         ("C-! C-c" . calendar)
         ("C-x <f11>" . calendar)))

(use-package calculator
  :bind (("C-! n" . calculator)
         ("C-! C-n" . calculator)
         ("C-x <f12>" . calculator)))

(use-package compile
  :bind (("C-! k" . compile)
         ("C-! C-k" . compile)
         ("C-x <f5>" . compile)
         ("M-<f5>" . recompile)
         ("C-x a k" . recompile)
         ("C-x C-a C-k" . recompile)))

(use-package linum
  :bind (("C-' n" . linum-mode)
         ("C-' C-n" . linum-mode)
         ("C-<f6>" . linum-mode)))

(use-package simple
  :bind (("C-' w" . toggle-truncate-lines)
         ("C-' C-w" . toggle-truncate-lines)
         ("C-<f9>" . toggle-truncate-lines)
         ("C-' q" . auto-fill-mode)
         ("C-' C-q" . auto-fill-mode)
         ("C-c C-\\" . just-one-space)
         ("C-c \\" . just-one-space))
  :config
  (custom-set-variables
   '(auto-save-file-name-transforms `((".*" ,backup-dir t)))
   '(auto-save-list-file-prefix backup-dir)
   '(backup-directory-alist `((".*" . ,backup-dir)))
   '(create-lockfiles nil))
  (column-number-mode 1))

(use-package util
  :load-path "lisp/lib/"
  :bind (("C-! e" . split-and-eshell)
         ("C-! C-e" . split-and-eshell)
         ("C-! p" . list-processes-and-switch)
         ("C-! C-p" . list-processes-and-switch)
         ("C-%" . match-paren)
         ("C-+" . w-resize)
         ("C-s" . save-buffer)
         ("C-x '" . switch-to-scratch)
         ("C-x 9" . delete-single-window)
         ("C-x <C-M-return>" . find-user-init-file)
         ("C-x <f3>" . list-processes-and-switch)
         ("C-x <f7>" . split-and-eshell)
         ("C-x C-'" . switch-to-scratch-other-window)
         ("C-x C-0" . delete-window)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-9" . delete-single-window)
         ("C-x C-c" . confirm-and-kill-terminal)
         ("C-x C-s" . isearch-forward)
         ("C-x M-[" . previous-buffer)
         ("C-x M-]" . next-buffer)
         ("C-x \"" . switch-to-scratch-other-frame)
         ("C-x |" . find-user-init-file)
         ("C-|" . move-to-window)
         ("<f6>" . match-paren)
         :map emacs-lisp-mode-map
         ("C-<f10>" . split-and-eshell)
         ("C-! C-r" . split-and-eshell))
  :init
  (global-unset-key (kbd "C-z"))
  ;; isearch-mode-map
  (bind-key (kbd "<f3>") 'isearch-repeat-forward isearch-mode-map)
  (bind-key (kbd "S-<f3>") 'isearch-repeat-backward isearch-mode-map)
  ;; enable disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  :config
  (add-hook 'prog-mode-hook 'font-lock-comment-annotations)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (kill-line-utils-init)
  (lvd-load-dir "~/.emacs.d/lisp/var/")

  (unless (is-windows?)		;; Linux-only config
    (eval-after-load 'info
      '(progn
         (push "/opt/local/share/info" Info-default-directory-list)
         (push "~/.emacs.d/info" Info-default-directory-list))))

  (when (is-windows?)		;; Windows-only config
    (setq w32-get-true-file-attributes nil)
    (w32-send-sys-command 61488)))

(provide 'config-utils)
