;;; mod-vc.el --- Integration with version control tools  -*- lexical-binding: t; -*-

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
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-toggle-skip-similar))

(use-package magit
  :ensure t
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

(provide 'mod-vc)
