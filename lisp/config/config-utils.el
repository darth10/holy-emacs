;;; Configuration for utility packages

(use-package abbrev
  :defer 2)

(use-package anzu
  :ensure t
  :defer t
  :hook (after-init . global-anzu-mode))

(use-package findr
  :ensure t
  :defer 2)

(use-package flycheck
  :ensure t
  :defer 5)

(use-package gist
  :ensure t
  :defer 5)

(use-package esup
  :ensure t
  :defer 2)

(use-package inflections
  :ensure t)

(use-package smex
  :ensure t
  :defer 2)

(use-package restclient
  :ensure t
  :defer 5)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :defer 5)

(use-package docker
  :ensure t
  :defer 2
  :config
  (setq docker-container-shell-file-name "/bin/sh"))

(use-package dockerfile-mode
  :ensure t
  :defer 5)

(use-package handlebars-mode
  :ensure t)

(use-package wide-column
  :ensure t)

(use-package grep
  :bind (("M-s G" . grep)
         ("M-s g" . rgrep)))

(use-package vc-git
  :defer 2
  :bind (("C-: <f3>" . vc-git-grep)
         ("M-s :" . vc-git-grep)
         ("C-: M-s" . vc-git-grep)))

(use-package ag
  :ensure t
  :bind (("M-s a a" . ag)
         ("M-s a g" . ag-regexp)
         ("M-s a A" . ag-project)
         ("M-s a G" . ag-project-regexp)))

(use-package dired
  :bind (("C-x C-j" . dired-jump-other-window)
         :map  dired-mode-map
         ("C-x C-/" . wdired-change-to-wdired-mode))
  :config
  (use-package direx
    :ensure t
    :defer 2
    :bind ("C-c C-j" . direx:jump-to-directory-other-window)))

(use-package hideshow
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
  :defer 2
  :bind (("C-' C-y" . yas-global-mode)
         ("C-' y" . yas-global-mode))
  :config
  (use-package yasnippet-snippets
    :ensure t)

  (add-to-list 'yas-snippet-dirs "~/.emacs.d/data/snippets/")
  (yas-global-mode t))

(use-package editorconfig
  :ensure t
  :defer 2
  :config
  (editorconfig-mode 1))

(use-package lacarte
  :ensure t
  :bind ("ESC M-x" . lacarte-execute-menu-command))

(use-package woman
  :unless (core:is-windows-p)
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
  :defer 2
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package subword
  :bind (("C-' c" . subword-mode)
         ("C-' C-c" . subword-mode)))

(use-package recentf
  :bind (("C-c : ;" . recentf-open-files)
         ("C-c C-: C-;" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 40
        recentf-max-saved-items 40)
  (recentf-mode t))

(use-package fixme-mode
  :ensure t)

(use-package bm
  :ensure t
  :defer 2
  :bind (("C-c : #" . bm-remove-all-current-buffer)
         ("C-c : C-#" . bm-remove-all-current-buffer)
         ("C-c : c" . bm-show-all)
         ("C-c : C-c" . bm-show-all)
         ("C-c ," . bm-previous)
         ("C-c ." . bm-next)
         ("C-c : p" . bm-previous)
         ("C-c : C-p" . bm-previous)
         ("C-c : n" . bm-next)
         ("C-c : C-n" . bm-next)
         ("C-c : ." . bm-toggle)
         ("C-c : C-." . bm-toggle))
  :init
  (setq bm-restore-repository-on-load t
        bm-repository-file "~/.emacs.d/bookmarks")
  :config
  (defface +bm-fringe-face
    '((t (:foreground "SkyBlue")))
    "Face for bookmark fringe"
    :group 'bm)

  (setq bm-fringe-face '+bm-fringe-face
        bm-fringe-persistent-face '+bm-fringe-face
        bm-highlight-style 'bm-highlight-only-fringe
        bm-cycle-all-buffers t
        bm-buffer-persistence t)

  (bm-repository-load)

  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda ()
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  (advice-add 'bm-toggle :after 'bm-buffer-save))

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

(use-package clipmon
  :ensure t
  :defer 2
  :config
  (clipmon-mode-start))

(use-package exec-path-from-shell
  :ensure t
  :unless (core:is-windows-p)
  :defer 2
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<f3>" . mc/mark-all-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-x <C-return>" . mc/edit-lines)
         ("C-x RET RET" . set-rectangular-region-anchor))
  :init
  (face-spec-set 'mc/cursor-bar-face '((t (:height 1 :background "green")))))

(use-package ws-butler
  :ensure t
  :commands (modes/init-ws-butler-mode)
  :bind (("C-' d" . ws-butler-global-mode)
         ("C-' C-d" . ws-butler-global-mode))
  :init
  (ws-butler-global-mode t))

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package simple
  :bind (("C-' w" . toggle-truncate-lines)
         ("C-' C-w" . toggle-truncate-lines)
         ("C-<f9>" . toggle-truncate-lines)
         ("C-' q" . auto-fill-mode)
         ("C-' C-q" . auto-fill-mode)
         ("C-c C-\\" . just-one-space)
         ("C-c \\" . just-one-space))
  :config
  (defconst +simple-backup-dir
    (concat user-emacs-directory "var/backups"))
  (setq auto-save-file-name-transforms `((".*" ,+simple-backup-dir t))
        auto-save-list-file-prefix +simple-backup-dir
        backup-directory-alist `((".*" . ,+simple-backup-dir))
        create-lockfiles nil)
  (setq-default truncate-lines t)
  (column-number-mode 1))

(use-package isearch
  :bind (("M-s s" . isearch-forward)
         ("M-s r" . isearch-backward)
         :map isearch-mode-map
         ("<f3>" . isearch-repeat-forward)
         ("S-<f3>" . isearch-repeat-backward)))

(use-package util
  :load-path "lisp/lib/"
  :bind (("C-! e" . util/find-or-run-eshell)
         ("C-! C-e" . util/find-or-run-eshell)
         ("C-! p" . util/list-processes-and-switch)
         ("C-! C-p" . util/list-processes-and-switch)
         ("C-! s" . util/find-or-run-shell)
         ("C-! C-s" . util/find-or-run-shell)
         ("C-%" . util/match-paren)
         ("C-+" . util/resize-window)
         ("C-s" . save-buffer)
         ("C-x '" . util/switch-to-scratch)
         ("C-x 9" . util/delete-single-window)
         ("C-x <C-M-return>" . util/find-user-init-file)
         ("C-x <f3>" . util/list-processes-and-switch)
         ("C-x <f9>" . util/find-or-run-eshell)
         ("C-x C-'" . util/switch-to-scratch-other-window)
         ("C-x C-0" . delete-window)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-9" . util/delete-single-window)
         ("C-x C-c" . util/confirm-and-kill-terminal)
         ("C-x M-[" . previous-buffer)
         ("C-x M-]" . next-buffer)
         ("M-[" . tab-to-tab-stop)
         ("C-x \"" . util/switch-to-scratch-other-frame)
         ("C-x |" . util/find-user-init-file)
         ("C-|" . util/switch-to-window)
         ("<f6>" . util/match-paren)
         :map emacs-lisp-mode-map
         ("C-<f10>" . util/find-or-run-eshell)
         ("C-! C-r" . util/find-or-run-eshell))
  :commands (util/kill-line-utils-init
             util/upgrade
             util/rebuild)
  :init
  (global-unset-key (kbd "C-z"))

  ;; enable disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (util/kill-line-utils-init)

  (when (core:is-windows-p)     ;; Windows-only config
    (setq w32-get-true-file-attributes nil)
    (w32-send-sys-command 61488)))

(use-package tramp
  :defer 2
  :config
  ;; File paths like `/sshx:user@remotehost|sudo:remotehost:/etc/dhcpd.conf`
  ;; will open remote files over multiple hops.
  (setq
   ;; useful variables for debugging tramp
   ;; tramp-debug-buffer t
   ;; tramp-verbose 9
   tramp-default-method "scpx"))

(provide 'config-utils)
