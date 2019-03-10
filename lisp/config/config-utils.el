;;; Configuration for utility packages

(use-package flycheck
  :ensure t
  :defer 5)

(use-package gist
  :ensure t
  :defer 5)

(use-package esup
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
              ("C-c v d" . hs-hide-all)
              ("C-c s" . hs-show-block)
              ("C-c v s" . hs-show-all))
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
  (custom-set-faces
   '(yas-field-highlight-face ((t (:inherit 'region)))))

  (let* ((temp-yas-snippet-dirs
          (append yas-snippet-dirs
                  (list (expand-file-name (concat core-var-dir-path "snippets")
                                          user-emacs-directory))))
         (temp-yas-snippet-dirs
          (delete yas--default-user-snippets-dir temp-yas-snippet-dirs)))
    (setq yas-snippet-dirs temp-yas-snippet-dirs))

  (use-package yasnippet-snippets
    :ensure t)

  (yas-global-mode t))

(use-package editorconfig
  :ensure t
  :defer 2
  :config
  (editorconfig-mode 1))

(use-package woman
  :unless (core:is-windows-p)
  :bind ("C-x ?" . woman))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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
        recentf-max-saved-items 40
        recentf-save-file (concat core-var-cache-dir-full-path "recentf"))
  (recentf-mode t))

(use-package fic-mode
  :ensure t
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "HACK"))
  (add-hook 'prog-mode-hook #'fic-mode)
  (add-hook 'conf-mode-hook #'fic-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package calendar
  :bind (("C-! c" . calendar)
         ("C-! C-c" . calendar)
         ("C-x <f11>" . calendar)))

(use-package calculator
  :bind (("C-! n" . calculator)
         ("C-! C-n" . calculator)
         ("C-x <f12>" . calculator))
  :config
  ;; increase size of calculator window
  (advice-add 'calculator :after #'(lambda () (enlarge-window 2))))

(use-package try
  :ensure t
  :defer 2)

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
  :bind (("C-' d" . ws-butler-mode)
         ("C-' C-d" . ws-butler-mode))
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'conf-mode-hook #'ws-butler-mode))

(use-package desktop
  :config
  (desktop-save-mode t))

(provide 'config-utils)
