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

(provide 'config-files)
