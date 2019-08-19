;;; mod-editor.el --- Primary editor configuration   -*- lexical-binding: t; -*-

;;; config-editor.el -*- lexical-binding: t; -*-

(custom-set-variables   ;; Set color theme
 `(custom-enabled-themes (quote ,core-enabled-custom-themes)))

(use-package core-editor
  :load-path core-lib-path
  :bind (("C-' n" . core/display-line-numbers)
         ("C-' C-n" . core/display-line-numbers)
         ("C-<f6>" . core/display-line-numbers)
         ("C-! e" . core/find-or-run-eshell)
         ("C-! C-e" . core/find-or-run-eshell)
         ("C-! p" . core/list-processes-and-switch)
         ("C-! C-p" . core/list-processes-and-switch)
         ("C-! s" . core/find-or-run-shell)
         ("C-! C-s" . core/find-or-run-shell)
         ("C-%" . core/match-paren)
         ("C-+" . core/resize-window)
         ("C-s" . save-buffer)
         ("C-x <C-M-return>" . core/find-user-init-file)
         ("C-x <f3>" . core/list-processes-and-switch)
         ("C-x C-0" . delete-window)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-5 C-0" . delete-frame)
         ("C-x C-5 C-1" . delete-other-frames)
         ("C-x C-5 C-2" . make-frame-command)
         ("C-x 9" . core/delete-single-window)
         ("C-x C-9" . core/delete-single-window)
         ("C-x '" . core/switch-to-scratch-other-window)
         ("C-x C-'" . core/switch-to-scratch-other-window)
         ("C-x \"" . core/switch-to-scratch)
         ("C-x C-\"" . core/switch-to-scratch)
         ("C-x 5 '" . core/switch-to-scratch-other-frame)
         ("C-x C-5 C-'" . core/switch-to-scratch-other-frame)
         ("C-x C-c" . core/confirm-and-kill-terminal)
         ("C-x M-[" . previous-buffer)
         ("C-x M-]" . next-buffer)
         ("M-[" . tab-to-tab-stop)
         ("C-x |" . core/find-user-init-file)
         ("C-|" . core/switch-to-window)
         ("<f6>" . core/match-paren)
         ("M-<down>" . core/move-line-region-down)
         ("M-<up>" . core/move-line-region-up)
         ("M-n" . core/move-line-region-down)
         ("M-p" . core/move-line-region-up)
         ("C-' w" . toggle-truncate-lines)
         ("C-' C-w" . toggle-truncate-lines)
         ("C-<f9>" . toggle-truncate-lines)
         ("C-' q" . auto-fill-mode)
         ("C-' C-q" . auto-fill-mode)
         ("C-c \\" . just-one-space)
         ("C-h C-l" . describe-personal-keybindings))
  :commands (core:kill-line-utils-init)
  :init
  (global-unset-key (kbd "<f10>"))
  (global-unset-key (kbd "C-z"))
  (setq shell-command-switch "-ic")

  ;; enable disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (core:kill-line-utils-init)

  (when (core:is-windows-p)     ;; Windows-only config
    (setq w32-get-true-file-attributes nil)
    (w32-send-sys-command 61488)))

(use-package whitespace
  :bind (("C-' ." . whitespace-mode)
         ("C-' C-." . whitespace-mode)))

(use-package simple
  :config
  (defconst +simple-backup-dir
    (concat user-emacs-directory (concat core-var-cache-dir-path "backups/")))
  (setq auto-save-file-name-transforms `((".*" ,+simple-backup-dir t))
        auto-save-list-file-prefix +simple-backup-dir
        backup-directory-alist `((".*" . ,+simple-backup-dir))
        create-lockfiles nil))

(use-package tramp
  :defer 2
  :config
  ;; File paths like `/sshx:user@remotehost|sudo:remotehost:/etc/dhcpd.conf`
  ;; will open remote files over multiple hops.
  (setq
   ;; tramp-debug-buffer t
   ;; tramp-verbose 9
   tramp-default-method "scpx"
   tramp-persistency-file-name (concat core-var-cache-dir-full-path "tramp")
   tramp-auto-save-directory (concat core-var-cache-dir-full-path "tramp-auto-save/")))

(use-package gud
  :defer t
  :lang (:map gud-minor-mode-map
         (:debug-set-break . gud-break)
         (:debug-remove-break . gud-remove)
         (:debug-step-over . gud-next)
         (:debug-step-into . gud-step)
         (:debug-step-out . gud-finish)
         (:debug-continue . gud-cont)
         (:debug-run . gud-run))
  :config
  (setq gdb-many-windows t))

(use-package desktop
  :init
  (setq desktop-path (list core-var-cache-dir-full-path)
        desktop-base-file-name "desktop"
        desktop-base-lock-name "desktop.lock")
  :config
  (desktop-save-mode t))

(use-package god-mode
  :ensure t
  :if core-enable-god-mode
  :bind (("<escape>" . god-local-mode)
         ("S-<escape>" . god-mode-all)
         ("M-i" . god-local-mode)
         :map god-local-mode-map
         ("." . repeat)
         ("z" . repeat)
         ("i" . god-local-mode))
  :hook (after-init . god-mode-all)
  :config
  (defun +god--toggle-on-overwrite ()
    (if (bound-and-true-p overwrite-mode)
        (god-local-mode-pause)
      (god-local-mode-resume)))

  (let* ((exempt-modes (list
                        'Custom-mode
                        'Info-mode
                        'ag-mode
                        'calendar-mode
                        'calculator-mode
                        'cider-test-report-mode
                        'compilation-mode
                        'debugger-mode
                        'dired-mode
                        'edebug-mode
                        'ediff-mode
                        'eww-mode
                        'geben-breakpoint-list-mode
                        'ibuffer-mode
                        'org-agenda-mode
                        'recentf-dialog-mode
                        'wdired-mode
                        )))
    (dolist (i exempt-modes)
      (add-to-list 'god-exempt-major-modes i)))

  (add-hook 'overwrite-mode-hook #'+god--toggle-on-overwrite))

(use-package transient
  :ensure t
  :defer 2
  :init
  (let ((transient-dir-path (concat core-var-cache-dir-full-path "transient/")))
    (setq transient-levels-file (concat transient-dir-path "levels.el")
          transient-values-file (concat transient-dir-path "values.el")
          transient-history-file (concat transient-dir-path "history.el"))))

(use-package exec-path-from-shell
  :ensure t
  :unless (core:is-windows-p)
  :defer 2
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package projectile
  :ensure t
  :defer 2
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :init
  (setq projectile-cache-file (concat core-var-cache-dir-full-path "projectile/cache")
        projectile-known-projects-file (concat core-var-cache-dir-full-path "projectile/bookmarks.eld"))
  :config
  (projectile-mode t))

(use-package docker
  :ensure t
  :defer 2
  :config
  (setq docker-container-shell-file-name "/bin/sh"))

(use-package server
  :ensure t
  :defer 2
  :config
  (setq server-auth-dir (concat core-var-cache-dir-full-path "server/"))
  (server-start))

(use-package edit-server
  :ensure t
  :if window-system
  :defer 2
  :config
  (edit-server-start))

(use-package clipmon
  :ensure t
  :defer 2
  :config
  (clipmon-mode-start))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c <f3>" . mc/mark-all-like-this)
         ("C-c >" . mc/mark-all-like-this)
         ("C-x <C-return>" . mc/edit-lines)
         ("C-x RET RET" . set-rectangular-region-anchor))
  :init
  (setq mc/list-file (concat core-var-cache-dir-full-path "mc-lists.el"))
  (face-spec-set 'mc/cursor-bar-face '((t (:height 1 :background "green")))))

(use-package mod-editor-compile    :load-path core-modules-lib-path)
(use-package mod-editor-completion :load-path core-modules-lib-path)
(use-package mod-editor-format     :load-path core-modules-lib-path)
(use-package mod-editor-help       :load-path core-modules-lib-path)
(use-package mod-editor-highlight  :load-path core-modules-lib-path)
(use-package mod-editor-navigation :load-path core-modules-lib-path)
(use-package mod-editor-parens     :load-path core-modules-lib-path)
(use-package mod-editor-search     :load-path core-modules-lib-path)

(provide 'mod-editor)
