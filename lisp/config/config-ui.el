;;; Configuration for UI/theme -*- lexical-binding: t; -*-

(use-package solarized-theme
  :ensure t
  :init

  (defconst +theme-scratch-message-logo-text
    "
      __          __
     / /_  ____  / /_  __    ___  ____ ___  ____ ___________
    / __ \\/ __ \\/ / / / /   / _ \\/ __ `__ \\/ __ `/ ___/ ___/
   / / / / /_/ / / /_/ /   /  __/ / / / / / /_/ / /__(__  )
  /_/ /_/\\____/_/\\__, /    \\___/_/ /_/ /_/\\__,_/\\___/____/
                /____/


")

  (defconst +theme-scratch-message-help-text
    "To open a file, type C-x C-f.
To display all available key bindings, type C-h C-l.
To quit a partially entered command, type C-g.
To quit Emacs, type C-x C-c.

For information about GNU Emacs and the GNU system, type C-h C-a.")

  (defun +theme-get-scratch-message ()
    (let* ((face-for-logo 'font-lock-function-name-face)
           (face-for-keys 'font-lock-string-face)
           (face-for-comments 'font-lock-comment-delimiter-face)
           (version-text (concat
                          (propertize (format "holy-emacs %s"
                                              holy-emacs-version)
                                      'face face-for-logo)
                          " / "
                          (propertize (format "GNU Emacs %s"
                                              emacs-version)
                                      'face face-for-logo)))
           (help-text (replace-regexp-in-string
                       "C-." (lambda (s) (propertize s 'face face-for-keys))
                       (concat +theme-scratch-message-help-text "\n"
                               version-text))))
      (concat
       (replace-regexp-in-string
        "^" (propertize ";; " 'face face-for-comments)
        (propertize +theme-scratch-message-logo-text 'face face-for-logo))
       (replace-regexp-in-string
        "^" (propertize ";; " 'face face-for-comments)
        help-text)
       "\n\n")))

  ;;; vars set before loading theme
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t
        ansi-color-names-vector
        ["#242424" "#e5786d" "#95e454" "#cae682"
         "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])

  (custom-set-variables
   '(custom-enabled-themes (quote (solarized-dark)))
   '(custom-safe-themes
     (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
             default))))

  ;;; vars set after loading theme
  (setq fancy-splash-image nil
        inhibit-default-init t
        inhibit-startup-screen t
        split-height-threshold 40
        split-width-threshold nil
        initial-scratch-message (+theme-get-scratch-message))

  :config

  (face-spec-set 'escape-glyph '((t (:foreground "#ddaa6f" :weight bold))))
  (face-spec-set 'header-line '((((class color) (min-colors 89)) (:background "#303030" :foreground "#e7f6da"))))
  (face-spec-set 'helm-ff-directory '((t (:background "LightGray" :foreground "black"))))
  (face-spec-set 'helm-swoop-target-word-face '((t (:foreground "green"))))
  (face-spec-set 'lazy-highlight '((((class color) (min-colors 89)) (:background "#384048" :foreground "#a0a8b0"))))
  (face-spec-set 'minibuffer-prompt '((t (:foreground "green"))))
  ;; #073642 is solarized active mode-line color
  (face-spec-set 'mode-line '((nil (:box nil :overline "#073642" :underline "#073642"))))
  (face-spec-set 'mode-line-inactive '((nil (:box nil :background "#073642" :overline "#073642" :underline "#073642"))))
  (face-spec-set 'region '((t (:background "white" :foreground "black"))))
  (face-spec-set 'show-paren-match '((t (:background "Dodgerblue1" :foreground "white" :weight extra-bold))))

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
    (face-spec-set 'default '((t (:height 148 :weight normal :width normal))))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (setq rainbow-delimiters-highlight-braces-p nil
        rainbow-delimiters-highlight-brackets-p nil)
  :config
  (face-spec-set 'rainbow-delimiters-depth-1-face '((t (:foreground "light sky blue"))))
  (face-spec-set 'rainbow-delimiters-depth-2-face '((t (:foreground "royal blue"))))
  (face-spec-set 'rainbow-delimiters-depth-3-face '((t (:foreground "medium orchid"))))
  (face-spec-set 'rainbow-delimiters-depth-4-face '((t (:foreground "dark orange"))))
  (face-spec-set 'rainbow-delimiters-depth-5-face '((t (:foreground "yellow"))))
  (face-spec-set 'rainbow-delimiters-depth-6-face '((t (:foreground "lawn green"))))
  (face-spec-set 'rainbow-delimiters-depth-7-face '((t (:foreground "light sky blue"))))
  (face-spec-set 'rainbow-delimiters-depth-8-face '((t (:foreground "royal blue"))))
  (face-spec-set 'rainbow-delimiters-depth-9-face '((t (:foreground "royal blue"))))
  (face-spec-set 'rainbow-delimiters-unmatched-face '((t (:foreground "medium orchid"))))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package yascroll
  :ensure t
  :config
  (face-spec-set 'yascroll:thumb-fringe '((t (:background "lawn green" :foreground "lawn green"))))
  (global-yascroll-bar-mode 1))

(use-package highlight
  :ensure t
  :defer 2
  :config
  (face-spec-set 'highlight '((t (:background "#454545" :foreground "#ffffff")))))

(use-package hl-line
  :bind (("C-' l" . hl-line-mode)
         ("C-' C-l" . hl-line-mode)
         ("C-<f4>" . hl-line-mode))
  :init
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  :config
  (face-spec-set 'hl-line '((t (:background "gray27" :foreground "green")))))

(use-package highlight-symbol
  :ensure t
  :bind (("C-' ." . highlight-symbol-mode)
         ("C-' C-." . highlight-symbol-mode))
  :config
  (face-spec-set 'highlight-symbol-face '((t (:foreground "green"))))
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package display-line-numbers
  :unless (version< emacs-version "26.0.50")
  :bind (("C-' n" . display-line-numbers-mode)
         ("C-' C-n" . display-line-numbers-mode)
         ("C-<f6>" . display-line-numbers-mode))
  :commands (display-line-numbers-mode)
  :init
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'conf-mode-hook 'display-line-numbers-mode))

;; Emacs versions less than 26.1 will have to use nlinum-mode
;; for line numbers. There's a few minor issues it has with edebug.
(use-package nlinum
  :ensure t
  :if (version< emacs-version "26.0.50")
  :bind (("C-' n" . nlinum-mode)
         ("C-' C-n" . nlinum-mode)
         ("C-<f6>" . nlinum-mode))
  :commands (nlinum-mode)
  :init
  (defun +nlinum-refresh ()
    (when nlinum-mode
      (nlinum-mode)))

  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'conf-mode-hook 'nlinum-mode)

  :config
  (defconst +nlinum-line-number-lpad 4
    "Left padding for line numbers.")
  (defconst +nlinum-line-number-rpad 1
    "Right padding for line numbers.")
  (defconst +nlinum-line-number-pad-char 32
    "Padding character for line numbers.")

  (defun +nlinum-format-fn (line width)
    "Formatting function for `nlinum-format-function'."
    (let ((str (number-to-string line)))
      (setq str (concat (make-string (max 0 (- +nlinum-line-number-lpad (length str)))
                                     +nlinum-line-number-pad-char)
                        str
                        (make-string +nlinum-line-number-rpad +nlinum-line-number-pad-char)))
      (put-text-property 0 (length str) 'face 'linum str)
      str))

  (setq nlinum-format-function '+nlinum-format-fn)

  (use-package nlinum-hl
    :ensure t
    :if (version< emacs-version "26.0.50")
    :config
    (advice-add 'set-frame-font :after 'nlinum-hl-flush-all-windows)))

(use-package diff-hl
  :ensure t
  :defer 2
  :config
  (setq diff-hl-margin-symbols-alist '((insert . " ")
                                       (delete . " ")
                                       (change . " ")
                                       (unknown . " ")
                                       (ignored . " ")))

  (global-diff-hl-mode 1)
  (diff-hl-margin-mode t)

  (face-spec-set 'diff-hl-insert '((t (:background "ForestGreen" :foreground "ForestGreen"))))
  (face-spec-set 'diff-hl-change '((t (:background "DimGray" :foreground "DimGray"))))
  (face-spec-set 'diff-hl-delete '((t (:background "Orangered3" :foreground "Orangered3"))))

  ;; Since both nlinum and diff-hl use the margin, diff-hl
  ;; seems to overwrite line numbers. To get around this,
  ;; enable nlinum-mode again if it's active.
  (use-package nlinum
    :if (version< emacs-version "26.0.50")
    :config
    (+nlinum-refresh))

  (use-package magit
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'config-ui)
