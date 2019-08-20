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
         ("C-: C-d" . vc-diff)
         ("C-: C-c C-d" . git-diff-tree)
         ("C-: C-l" . magit-log-current)
         ("C-: C-k" . magit-run-gitk))
  :hook (magit-status-mode . +vc--magit-status-mode-setup)
  :config
  (defun git-diff-tree ()
    (interactive)
    (magit-diff-working-tree "HEAD"))

  (defun +vc--magit-status-mode-setup ()
    (local-unset-key (kbd "x"))
    (local-unset-key (kbd ":")))

  (setq magit-auto-revert-mode t
        magit-last-seen-setup-instructions "1.4.0"))

(provide 'mod-vc)
