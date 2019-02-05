;;; config-editor.el -*- lexical-binding: t; -*-

(use-package core-editor
  :load-path core-lib-path
  :bind (("C-' n" . core/display-line-numbers)
         ("C-' C-n" . core/display-line-numbers)
         ("C-<f6>" . core/display-line-numbers)
         ("C-! e" . core/find-or-run-eshell)
         ("C-! C-e" . core/find-or-run-eshell)
         ("C-! p" . core/list-processes-and-switch)
         ("C-! C-p" . core/list-processes-and-switch)
         ("C-! s" . core/find-or-run-shell)
         ("C-! C-s" . core/find-or-run-shell)
         ("C-%" . core/match-paren)
         ("C-+" . core/resize-window)
         ("C-s" . save-buffer)
         ("C-x <C-M-return>" . core/find-user-init-file)
         ("C-x <f3>" . core/list-processes-and-switch)
         ("C-x C-0" . delete-window)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-5 C-0" . delete-frame)
         ("C-x C-5 C-1" . delete-other-frames)
         ("C-x C-5 C-2" . make-frame-command)
         ("C-x 9" . core/delete-single-window)
         ("C-x C-9" . core/delete-single-window)
         ("C-x '" . core/switch-to-scratch-other-window)
         ("C-x C-'" . core/switch-to-scratch-other-window)
         ("C-x \"" . core/switch-to-scratch)
         ("C-x C-\"" . core/switch-to-scratch)
         ("C-x 5 '" . core/switch-to-scratch-other-frame)
         ("C-x C-5 C-'" . core/switch-to-scratch-other-frame)
         ("C-x C-c" . core/confirm-and-kill-terminal)
         ("C-x M-[" . previous-buffer)
         ("C-x M-]" . next-buffer)
         ("M-[" . tab-to-tab-stop)
         ("C-x |" . core/find-user-init-file)
         ("C-|" . core/switch-to-window)
         ("<f6>" . core/match-paren)
         ("M-<down>" . core/move-line-region-down)
         ("M-<up>" . core/move-line-region-up)
         ("M-n" . core/move-line-region-down)
         ("M-p" . core/move-line-region-up))
  :commands (core/kill-line-utils-init)
  :init
  (global-unset-key (kbd "C-z"))

  ;; enable disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (core/kill-line-utils-init)

  (when (core:is-windows-p)     ;; Windows-only config
    (setq w32-get-true-file-attributes nil)
    (w32-send-sys-command 61488)))

(provide 'config-editor)
