;;; Configuration for god-mode, modeline, frame/cursor

(use-package god-mode
  :ensure t
  :diminish god-local-mode
  :bind (("<escape>" . god-local-mode)
         ("S-<escape>" . god-mode-all)
         ("M-i" . god-local-mode)
         :map god-local-mode-map
         ("." . repeat)
         ("z" . repeat)
         ("i" . god-local-mode))
  :init
  (god-mode-all)

  :config
  (defun god-toggle-on-overwrite ()
    (if (bound-and-true-p overwrite-mode)
        (god-local-mode-pause)
      (god-local-mode-resume)))

  (let* ((exempt-modes (list
                        'Custom-mode
                        'Info-mode
                        'ag-mode
                        'calendar-mode
                        'calculator-mode
                        'cider-test-report-mode
                        'compilation-mode
                        'debugger-mode
                        'dired-mode
						'edebug-mode
                        'ediff-mode
                        'eww-mode
                        'geben-breakpoint-list-mode
                        'ibuffer-mode
                        'org-agenda-mode
                        'recentf-dialog-mode
                        'wdired-mode
                        )))
    (dolist (i exempt-modes)
      (add-to-list 'god-exempt-major-modes i)))

  (add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config

  ;; modeline on Windows needs a bit more height
  (let* ((modeline-height (if (core/is-windows-p) 40 34)))
	(custom-set-variables
	 '(doom-modeline-height `,modeline-height)
	 '(doom-modeline-bar-width 11)))

  (defvar modeline-mode-string " ")
  (doom-modeline-def-segment cur-mode
    (if (doom-modeline--active)
        '(" " modeline-mode-string " ")
      "   "))
  (doom-modeline-def-modeline
   'main
   '(workspace-number window-number bar cur-mode matches buffer-info-simple buffer-position selection-info)
   '(debug buffer-encoding major-mode process vcs checker)))

(use-package frame
  :bind (("C-x C-5 C-0" . delete-frame)
         ("C-x C-5 C-1" . delete-other-frames)
         ("C-x C-5 C-2" . make-frame-command))
  :init
  (custom-set-variables
   '(echo-keystrokes 0.05)
   '(cursor-in-non-selected-windows nil))

  :config
  (defun configure-cursor ()
    (let* ((is-line-overflow
            (> (current-column) 70))
           (prev-cur-color (face-background 'cursor))
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
      (progn
        (unless (eq prev-cur-color cur-color)
          (set-cursor-color cur-color)
		  (set-face-attribute 'doom-modeline-bar nil :background cur-color)
		  (doom-modeline-refresh-bars))

        (setq cursor-type cur-type)
        (setq modeline-mode-string next-mode-string))))

  (custom-set-faces
   '(cursor ((t (:background "green")))))

  (add-hook 'post-command-hook 'configure-cursor)
  (blink-cursor-mode t))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :bind (("C-' k" . which-key-mode)
         ("C-' C-k" . which-key-mode))
  :init
  (which-key-setup-side-window-bottom)
  (which-key-enable-god-mode-support)
  (setq which-key-max-description-length 24)
  (setq which-key-max-display-columns 4)
  (unbind-key "C-h C-h")
  (which-key-mode t))

(use-package help-fns
  :bind (("C-h C-l" . describe-personal-keybindings)))

(provide 'config-modes)
