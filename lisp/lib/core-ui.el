;;; core-ui.el -*- lexical-binding: t; -*-

(defcustom core-enable-god-mode t
  "When nil, do not enable `god-mode` and relevant bindings."
  :type 'boolean
  :safe #'booleanp
  :group 'holy-emacs)

(defcustom core-enabled-custom-themes '(solarized-dark)
  "List of custom themes to enable by default. When nil, do not
use any custom theme."
  :type '(repeat symbol)
  :group 'holy-emacs)

(defface core-fringe-highlight-face '((t (:foreground "yellow")))
  "Face for the fringe bitmaps."
  :group 'holy-emacs)

(defconst core--default-font "Consolas"
  "Default font.")

(defconst core--scratch-message-logo-text
  "
      __          __
     / /_  ____  / /_  __    ___  ____ ___  ____ ___________
    / __ \\/ __ \\/ / / / /   / _ \\/ __ `__ \\/ __ `/ ___/ ___/
   / / / / /_/ / / /_/ /   /  __/ / / / / / /_/ / /__(__  )
  /_/ /_/\\____/_/\\__, /    \\___/_/ /_/ /_/\\__,_/\\___/____/
                /____/


")

(defconst core--scratch-message-help-text
  "To open a file, type C-x C-f.
To display all available key bindings, type C-h C-l.
To quit a partially entered command, type C-g.
To quit Emacs, type C-x C-c.

For information about GNU Emacs and the GNU system, type C-h C-a.")

(defvar core--modeline-mode-string ""
  "Current mode state for `doom-modeline`.")

(defvar core--display-line-numbers-function nil
  "Function to call for displaying line numbers.")

(defun core--get-scratch-message ()
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
                     (concat core--scratch-message-help-text "\n"
                             version-text))))
    (concat
     (replace-regexp-in-string
      "^" (propertize ";; " 'face face-for-comments)
      (propertize core--scratch-message-logo-text 'face face-for-logo))
     (replace-regexp-in-string
      "^" (propertize ";; " 'face face-for-comments)
      help-text)
     "\n\n")))

(defun core--configure-mode ()
  (let* ((is-line-overflow (> (current-column) 70))
         (prev-cur-color (face-background 'cursor))
         (prev-modeline-color (and (facep 'doom-modeline-bar)
                                   (face-background 'doom-modeline-bar)))
         (is-god-mode (and (boundp 'god-local-mode)
                           god-local-mode))
         (cur-color (cond (buffer-read-only "Gray")
                          (is-line-overflow "IndianRed")
                          (overwrite-mode "yellow")
                          (t "green")))
         (cur-type (cond (buffer-read-only 'box)
                         ((and overwrite-mode is-god-mode) 'hollow)
                         ((or is-god-mode overwrite-mode) 'box)
                         (t 'bar)))
         (next-mode-string (cond ((and overwrite-mode is-god-mode) "λ")
                                 (is-god-mode "λ")
                                 (overwrite-mode "!")
                                 (t " "))))
    (unless (eq prev-cur-color cur-color)
      (set-cursor-color cur-color))
    (when (and (facep 'doom-modeline-bar)
               (not (eq prev-modeline-color cur-color)))
      (set-face-attribute 'doom-modeline-bar nil :background cur-color)
      (doom-modeline-refresh-bars))

    (setq cursor-type cur-type)
    (setq core--modeline-mode-string next-mode-string)))

(use-package solarized-theme
  :ensure t)

;; This package is needed for eval-sexp-fu
(use-package highlight
  :ensure t
  :defer 2)

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

(use-package frame
  :bind (("C-x C-5 C-0" . delete-frame)
         ("C-x C-5 C-1" . delete-other-frames)
         ("C-x C-5 C-2" . make-frame-command)))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config

  (face-spec-set 'doom-modeline-panel '((t (:inherit mode-line-emphasis))))
  (face-spec-set 'doom-modeline-buffer-modified '((t (:inherit (warning bold) :background nil))))
  (face-spec-set 'doom-modeline-inactive-bar '((t (:inherit mode-line-emphasis))))

  ;; modeline on Windows needs a bit more height
  (let* ((modeline-height (if (core:is-windows-p) 40 34)))
    (setq doom-modeline-height modeline-height
          doom-modeline-bar-width 11))

  (doom-modeline-def-segment cur-mode
    (if (doom-modeline--active)
        '(" " core--modeline-mode-string " ")
      "   "))
  (doom-modeline-def-modeline
    'main
    '(workspace-number window-number bar cur-mode matches buffer-info-simple buffer-position selection-info)
    '(debug buffer-encoding major-mode process vcs checker)))

(defun core/display-line-numbers ()
  "Toggle displaying line numbers. This function simply calls the
value of `core--display-line-numbers-function`."
  (interactive)
  (call-interactively core--display-line-numbers-function))

(use-package display-line-numbers
  :unless (version< emacs-version "26.0.50")
  :commands (display-line-numbers-mode)
  :init
  (setq core--display-line-numbers-function #'display-line-numbers-mode))

;; Emacs versions less than 26.1 will have to use nlinum-mode
;; for line numbers. There's a few minor issues it has with edebug.
(use-package nlinum
  :ensure t
  :if (version< emacs-version "26.0.50")
  :commands (nlinum-mode)
  :init
  (defun core--refresh-nlinum ()
    (when nlinum-mode
      (nlinum-mode)))

  (setq core--display-line-numbers-function #'nlinum-mode)

  :config
  (defconst core--nlinum-line-number-lpad 4
    "Left padding for line numbers.")
  (defconst core--nlinum-line-number-rpad 1
    "Right padding for line numbers.")
  (defconst core--nlinum-line-number-pad-char 32
    "Padding character for line numbers.")

  (defun core--nlinum-format-fn (line width)
    "Formatting function for `nlinum-format-function'."
    (let ((str (number-to-string line)))
      (setq str (concat (make-string (max 0 (- core--nlinum-line-number-lpad (length str)))
                                     core--nlinum-line-number-pad-char)
                        str
                        (make-string core--nlinum-line-number-rpad core--nlinum-line-number-pad-char)))
      (put-text-property 0 (length str) 'face 'linum str)
      str))

  (setq nlinum-format-function 'core--nlinum-format-fn)

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
    (core--refresh-nlinum))

  (use-package magit
    :config
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (setq rainbow-delimiters-highlight-braces-p nil
        rainbow-delimiters-highlight-brackets-p nil)
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'conf-mode-hook #'rainbow-delimiters-mode))

(progn
  (custom-set-faces
   '(mode-line ((t (:box nil :overline nil :underline nil))))
   ;; #073642 is for solarized theme only
   '(mode-line-inactive ((t (:box nil :overline nil :underline nil)))))

  (face-spec-set 'show-paren-match '((t (:background "Dodgerblue1" :foreground "white" :weight extra-bold))))

  (when window-system
    (set-fringe-bitmap-face 'right-triangle 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'right-arrow 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'right-curly-arrow 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'left-triangle 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'left-arrow 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'left-curly-arrow 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'exclamation-mark 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'question-mark 'core-fringe-highlight-face)
    (set-fringe-bitmap-face 'empty-line 'core-fringe-highlight-face)

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1))

  (show-paren-mode 1)
  (blink-cursor-mode 1)

  ;;; Variables to set before loading theme
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t
        ansi-color-names-vector
        ["#242424" "#e5786d" "#95e454" "#cae682"
         "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])

  (custom-set-variables
   '(custom-safe-themes
     (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
             default))))
  (custom-set-variables
   `(custom-enabled-themes (quote ,core-enabled-custom-themes)))

  ;;; Variables to set after loading theme
  (setq fancy-splash-image nil
        inhibit-default-init t
        inhibit-startup-screen t
        split-height-threshold 40
        split-width-threshold nil
        echo-keystrokes 0.05
        cursor-in-non-selected-windows nil
        initial-scratch-message (core--get-scratch-message))

  (add-hook 'post-command-hook #'core--configure-mode)
  (add-hook 'prog-mode-hook core--display-line-numbers-function)
  (add-hook 'conf-mode-hook core--display-line-numbers-function)

  (bind-key "C-' n" #'core/display-line-numbers)
  (bind-key "C-' C-n" #'core/display-line-numbers)
  (bind-key "C-<f6>" #'core/display-line-numbers)

  ;; Customized font (other than core--default-font) will be
  ;; overwritten if the package-selected-packages custom variable
  ;; has not been written to custom-file.
  (when (and window-system (x-list-fonts core--default-font))
    (let ((font-height (if (core:is-windows-p) 134 148)))
      (custom-set-faces
       `(default ((t (:height ,font-height :family ,core--default-font :weight normal :width normal))))))))

(provide 'core-ui)
