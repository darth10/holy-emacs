;;; Configuration for UI/theme

(use-package solarized-theme
  :ensure t
  :init
  (custom-set-variables
   ;;; vars set before loading theme
   '(solarized-use-variable-pitch) nil
   '(solarized-scale-org-headlines nil)
   '(solarized-distinct-fringe-background t)
   '(x-underline-at-descent-line t)
   '(ansi-color-names-vector
     ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
   '(custom-enabled-themes (quote (solarized-dark)))
   '(custom-safe-themes
     (quote
      ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
   ;;; vars set after loading theme
   '(fancy-splash-image nil)
   '(hl-sexp-background-colors (quote ("gray27" "midnight blue")))
   '(inhibit-default-init t)
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(split-height-threshold 40)
   '(split-width-threshold nil)
   '(term-default-bg-color "#000000")
   '(term-default-fg-color "#00ff00"))
  :config
  (custom-set-faces
   '(default ((t (:foundry "monotype" :slant normal :weight normal :height 130 :width normal))))
   '(button ((t (:background "green" :foreground "black"))))
   '(escape-glyph ((t (:foreground "#ddaa6f" :weight bold))))
   '(header-line ((((class color) (min-colors 89)) (:background "#303030" :foreground "#e7f6da"))))
   '(helm-ff-directory ((t (:background "LightGray" :foreground "black"))))
   '(helm-swoop-target-word-face ((t (:foreground "green"))))
   '(isearch ((t (:background "green" :foreground "black"))))
   '(lazy-highlight ((((class color) (min-colors 89)) (:background "#384048" :foreground "#a0a8b0"))))
   '(minibuffer-prompt ((t (:foreground "green"))))
   '(mode-line ((t (:background "green1" :foreground "black"))))
   '(mode-line-buffer-id ((t (:background "green" :foreground "black" :weight bold))))
   '(mode-line-inactive ((t (:background "dimgray" :foreground "black"))))
   '(region ((t (:background "white" :foreground "black")))))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; custom font
(use-package font-utils
  :load-path "lisp/lib/"
  :config
  (progn
    (when (font-utils-exists-p config-custom-font)
      (set-frame-font config-custom-font nil t))))

(use-package paren
  :ensure t
  :config
  (set-face-background 'show-paren-match "Dodgerblue1")
  (set-face-foreground 'show-paren-match "white")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (show-paren-mode 1))

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
