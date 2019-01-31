;;; Configuration for UI/theme -*- lexical-binding: t; -*-

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
