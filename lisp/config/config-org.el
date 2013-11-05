;;; Configuration for Org mode

(defun configure-org ()
  (auto-complete-mode)
  (require 'org-agenda)
  (local-set-key (kbd "C-x t") 'org-timeline)
  (local-set-key (kbd "C-c l") 'org-store-link)
  (local-set-key (kbd "C-c c") 'org-capture)
  (local-set-key (kbd "C-c a") 'org-agenda))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'configure-org)

(provide 'config-org)
