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

(defface core-fringe-highlight-face '((t (:foreground "yellow")))
  "Face for the fringe bitmaps."
  :group 'holy-emacs)

(use-package solarized-theme
  :ensure t)

(progn
  (face-spec-set 'mode-line '((nil (:box nil :overline "#073642" :underline "#073642"))))
  (face-spec-set 'mode-line-inactive '((nil (:box nil :background "#073642" :overline "#073642" :underline "#073642"))))
  (face-spec-set 'show-paren-match '((t (:background "Dodgerblue1" :foreground "white" :weight extra-bold))))

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
  (scroll-bar-mode -1)
  (show-paren-mode 1)

  ;;; vars set before loading theme
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t
        ansi-color-names-vector
        ["#242424" "#e5786d" "#95e454" "#cae682"
         "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])

  (custom-set-variables
   `(custom-enabled-themes (quote ,core-enabled-custom-themes))
   '(custom-safe-themes
     (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
             default))))

  ;;; vars set after loading theme
  (setq fancy-splash-image nil
        inhibit-default-init t
        inhibit-startup-screen t
        split-height-threshold 40
        split-width-threshold nil
        initial-scratch-message (core--get-scratch-message))

  ;; customized font (other than core--default-font) will be
  ;; overwritten if the package-selected-packages custom variable
  ;; has not been written to custom-file.
  (when (if (null (x-list-fonts core--default-font)) nil t)
    (let ((font-height (if (core:is-windows-p) 134 148)))
      (custom-set-faces
       `(default ((t (:height ,font-height :family ,core--default-font :weight normal :width normal))))))))

(provide 'core-ui)
