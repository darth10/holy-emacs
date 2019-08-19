;;; mod-files.el --- File management and handling tools  -*- lexical-binding: t; -*-

(use-package recentf
  :bind (("C-c : ;" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 40
        recentf-max-saved-items 40
        recentf-save-file (concat core-var-cache-dir-full-path "recentf"))
  (recentf-mode t))

(use-package dired
  :bind (("C-x j" . dired-jump-other-window)
         ("C-x C-j" . dired-jump-other-window)
         :map  dired-mode-map
         ("C-x C-/" . wdired-change-to-wdired-mode)))

(use-package direx
  :ensure t
  :defer 2
  :after dired
  :bind (("C-x ," . direx:jump-to-directory-other-window)
         ("C-x C-," . direx:jump-to-directory-other-window)))

(use-package image-dired
  :init
  (setq image-dired-dir (concat core-var-cache-dir-full-path "image-dired/")
        image-dired-db-file (concat image-dired-dir "db/db.el"))
  :defer 2)

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
        bm-repository-file (concat core-var-cache-dir-full-path "bookmarks"))
  (setq-default bm-buffer-persistence t)
  :hook (((find-file after-revert) . bm-buffer-restore)
         ((save-hook kill-buffer) . bm-buffer-save)
         (kill-emacs . +bm:save-all))
  :config
  (defface +bm-fringe-face
    '((t (:foreground "SkyBlue")))
    "Face for bookmark fringe"
    :group 'bm)

  (setq bm-fringe-face '+bm-fringe-face
        bm-fringe-persistent-face '+bm-fringe-face
        bm-highlight-style 'bm-highlight-only-fringe
        bm-cycle-all-buffers t)

  (defun +bm:save-all ()
    (bm-buffer-save-all)
    (bm-repository-save))

  (advice-add 'bm-toggle :after #'bm-buffer-save)
  (bm-repository-load))

(use-package pdf-tools
  :ensure t
  :unless (core:is-windows-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (unless noninteractive
    (pdf-tools-install t)))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat core-var-cache-dir-full-path "nov-places")))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

(provide 'mod-files)
