;;; Configuration for UI/theme

(use-package solarized-theme
  :ensure t
  :init
  (custom-set-variables
   ;;; vars set before loading theme
   '(solarized-use-variable-pitch nil)
   '(solarized-scale-org-headlines nil)
   '(x-underline-at-descent-line t)
   '(ansi-color-names-vector
     ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
   '(custom-enabled-themes (quote (solarized-dark)))
   '(custom-safe-themes
     (quote
      ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
   ;;; vars set after loading theme
   '(fancy-splash-image nil)
   '(inhibit-default-init t)
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(indent-tabs-mode t)
   '(split-height-threshold 40)
   '(split-width-threshold nil)
   '(term-default-bg-color "#000000")
   '(term-default-fg-color "#00ff00"))
  :config
  (custom-set-faces
   '(button ((t (:background "green" :foreground "black"))))
   '(diff-hl-insert ((t (:background "ForestGreen" :foreground "ForestGreen"))))
   '(diff-hl-change ((t (:background "DimGray" :foreground "DimGray"))))
   '(diff-hl-delete ((t (:background "Orangered3" :foreground "Orangered3"))))
   '(doom-modeline-panel ((t (:inherit mode-line-emphasis))))
   '(doom-modeline-buffer-modified ((t (:inherit (warning bold) :background nil))))
   '(doom-modeline-inactive-bar ((t (:inherit mode-line-emphasis :foreground "#073642" :background "#073642"))))
   '(escape-glyph ((t (:foreground "#ddaa6f" :weight bold))))
   '(header-line ((((class color) (min-colors 89)) (:background "#303030" :foreground "#e7f6da"))))
   '(helm-ff-directory ((t (:background "LightGray" :foreground "black"))))
   '(helm-swoop-target-word-face ((t (:foreground "green"))))
   '(isearch ((t (:background "green" :foreground "black"))))
   '(lazy-highlight ((((class color) (min-colors 89)) (:background "#384048" :foreground "#a0a8b0"))))
   '(minibuffer-prompt ((t (:foreground "green"))))
   ;; #073642 is solarized active mode-line color
   '(mode-line ((nil (:box nil :overline "#073642" :underline "#073642"))))
   '(mode-line-inactive ((nil (:box nil :background "#073642" :overline "#073642" :underline "#073642"))))
   '(region ((t (:background "white" :foreground "black"))))
   '(show-paren-match ((t (:background "Dodgerblue1" :foreground "white" :weight extra-bold)))))

  (defface fringe-highlight-face
    '((t (:foreground "yellow")))
    "Face for the fringe bitmaps"
    :group 'basic-faces)
  (set-fringe-bitmap-face 'right-triangle 'fringe-highlight-face)
  (set-fringe-bitmap-face 'right-arrow 'fringe-highlight-face)
  (set-fringe-bitmap-face 'right-curly-arrow 'fringe-highlight-face)
  (set-fringe-bitmap-face 'left-triangle 'fringe-highlight-face)
  (set-fringe-bitmap-face 'left-arrow 'fringe-highlight-face)
  (set-fringe-bitmap-face 'left-curly-arrow 'fringe-highlight-face)
  (set-fringe-bitmap-face 'exclamation-mark 'fringe-highlight-face)
  (set-fringe-bitmap-face 'question-mark 'fringe-highlight-face)
  (set-fringe-bitmap-face 'empty-line 'fringe-highlight-face)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1))

(use-package font-utils
  :ensure t
  :config
  (defconst config-custom-font "Consolas")
  (when (font-utils-exists-p config-custom-font)
    (set-frame-font config-custom-font nil t)
    (add-to-list 'default-frame-alist
                 '(font . "-MS  -Consolas-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"))
    (custom-set-faces
     '(default ((t (:height 148 :weight normal :width normal)))))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (custom-set-variables
   '(rainbow-delimiters-highlight-braces-p nil)
   '(rainbow-delimiters-highlight-brackets-p nil))
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "light sky blue"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "royal blue"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "medium orchid"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "light sky blue"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "royal blue"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "royal blue"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "medium orchid")))))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package yascroll
  :ensure t
  :config
  (custom-set-faces
   '(yascroll:thumb-fringe ((t (:background "lawn green" :foreground "lawn green")))))
  (global-yascroll-bar-mode 1))

(use-package highlight
  :ensure t
  :defer 2
  :config
  (custom-set-faces
   '(highlight ((t (:background "#454545" :foreground "#ffffff"))))))

(use-package hl-line
  :bind (("C-' l" . hl-line-mode)
         ("C-' C-l" . hl-line-mode)
         ("C-<f4>" . hl-line-mode))
  :init
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  :config
  (custom-set-faces
   '(hl-line ((t (:background "gray27" :foreground "green"))))))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :bind (("C-' ." . highlight-symbol-mode)
         ("C-' C-." . highlight-symbol-mode))
  :config
  (custom-set-faces
   '(highlight-symbol-face ((t (:foreground "green")))))
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(provide 'config-ui)
