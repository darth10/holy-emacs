;;; Configuration for Org mode

(defun configure-org ()
  ;; (auto-complete-mode)
  (require 'org-agenda)
  (hl-line-mode t)
  (local-unset-key (kbd "C-'"))
  (local-unset-key (kbd "C-c :"))
  (local-unset-key (kbd "C-c ;"))
  (local-unset-key (kbd "C-c \\"))
  (local-unset-key (kbd "C-c a"))
  (local-unset-key (kbd "C-c C-j"))
  (local-unset-key (kbd "C-c C-n"))
  (local-unset-key (kbd "C-c C-p"))
  (local-unset-key (kbd "C-c C-b"))
  (local-set-key (kbd "C-c t") 'org-timeline)
  (local-set-key (kbd "C-c C-t") 'org-timeline)
  (local-set-key (kbd "C-c l") 'org-store-link)
  (local-set-key (kbd "C-c c") 'org-capture)
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-x C-e") (kbd "C-u C-c *"))
  (local-set-key (kbd "M-p") 'org-metaup)
  (local-set-key (kbd "M-n") 'org-metadown))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'configure-org)

(provide 'config-org)
