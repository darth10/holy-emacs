;;; Configuration for file management

(use-package recentf
  :bind (("C-c : ;" . recentf-open-files)
         ("C-c C-: C-;" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 40
        recentf-max-saved-items 40
        recentf-save-file (concat core-var-cache-dir-full-path "recentf"))
  (recentf-mode t))

(use-package dired
  :bind (("C-x C-j" . dired-jump-other-window)
         :map  dired-mode-map
         ("C-x C-/" . wdired-change-to-wdired-mode))
  :config
  (use-package direx
    :ensure t
    :defer 2
    :bind ("C-c C-j" . direx:jump-to-directory-other-window)))

(use-package image-dired
  :init
  (setq image-dired-dir (concat core-var-cache-dir-full-path "image-dired/")
        image-dired-db-file (concat image-dired-dir "db/db.el"))
  :defer 2)

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :ensure t
  :unless (core:is-windows-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive
    (pdf-tools-install t)))

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
  :config
  (defface +bm-fringe-face
    '((t (:foreground "SkyBlue")))
    "Face for bookmark fringe"
    :group 'bm)

  (setq bm-fringe-face '+bm-fringe-face
        bm-fringe-persistent-face '+bm-fringe-face
        bm-highlight-style 'bm-highlight-only-fringe
        bm-cycle-all-buffers t)

  (bm-repository-load)

  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda ()
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  (advice-add 'bm-toggle :after 'bm-buffer-save))

(provide 'config-files)
