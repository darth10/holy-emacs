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
  (add-to-list 'default-mode-line-format
               (quote (:eval (propertize (if (and (boundp 'god-local-mode) god-local-mode) "^" " ")))))
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
           (is-god-mode
            (and (boundp 'god-local-mode)
                 god-local-mode))
           (cur-color
            (cond (buffer-read-only "Gray")
                  (is-line-overflow "IndianRed")
                  (overwrite-mode "yellow")
                  (t "green")))
           (cur-type
            (cond (buffer-read-only 'box)
                  ((and overwrite-mode
                        is-god-mode)
                   'hollow)
                  ((or is-god-mode
                       overwrite-mode)
                   'box)
                  (t 'bar))))
      (progn
        (setq cursor-type cur-type)
        (set-cursor-color cur-color)
        (set-face-background 'mode-line cur-color)
        (set-face-attribute 'mode-line-buffer-id nil :background cur-color))))

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

(provide 'config-modes)
