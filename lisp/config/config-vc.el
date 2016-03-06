;;; Configuration for version control

(use-package ediff
  :bind (("C-! +" . ediff)
         ("C-! =" . ediff-buffers)
         ("C-! C-+" . ediff)
         ("C-! C-=" . ediff-buffers)
         ("C-x <f10>" . ediff-buffers)
         ("C-x S-<f10>" . ediff)
         ("C-: <f10>" . vc-ediff)
         ("C-: C-=" . vc-ediff)
         :map ediff-mode-map
         ("M-<down>" . ediff-next-difference)
         ("M-<up>" . ediff-previous-difference)
         ("M-<right>" . ediff-copy-A-to-B)
         ("M-<left>" . ediff-copy-B-to-A))
  :config
  (custom-set-variables
   '(ediff-split-window-function (quote split-window-horizontally))
   '(ediff-window-setup-function (quote ediff-setup-windows-plain)))
  (ediff-toggle-skip-similar))

(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind (("C-: :" . magit-status)
         ("C-: C-:" . magit-status)
         ("C-: C-s" . magit-status)
         ("C-: C-l" . magit-log-current)
         ("C-: C-k" . magit-run-gitk))
  :config

  (defun git-diff-tree ()
    (interactive)
    (magit-diff-working-tree "HEAD"))

  (defun configure-magit-status-mode ()
    (local-unset-key (kbd "x"))
    (local-unset-key (kbd ":")))

  (setq magit-auto-revert-mode t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (bind-key "C-: C-d" 'vc-diff)
  (bind-key "C-: C-c C-d" 'git-diff-tree)
  (add-hook 'magit-status-mode-hook 'configure-magit-status-mode))

(use-package diff-hl
  :ensure t
  :config

  (defun interactive-diff-hl-update ()
    (interactive)
    (diff-hl-update)
    (message "Refreshed diff"))

  (defun diff-hl-update-each-buffer ()
    (interactive)
    (mapc (lambda (buffer)
            (condition-case nil
                (with-current-buffer buffer
                  (diff-hl-update))
              (buffer-read-only nil)))
          (buffer-list)))

  (bind-key "C-x r =" 'interactive-diff-hl-update)
  (global-diff-hl-mode 1)

  (use-package magit
    :config
    (defadvice magit-refresh (after my-magit-refresh activate)
      (progn (diff-hl-update-each-buffer)))))

(provide 'config-vc)
