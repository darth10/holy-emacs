;;; Configuration for org-mode

(use-package org
  :ensure t
  :pin org-elpa
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c b" . org-iswitchb)
         ("C-c C-b" . org-iswitchb)
         :map org-mode-map
         ("C-c n" . org-agenda)
         ("C-c C-n" . org-agenda)
         ("C-c l" . org-store-link)
         ("M-p" . org-metaup)
         ("M-n" . org-metadown))
  :config
  (bind-key "C-x C-e" (kbd "C-u C-c *") org-mode-map)
  (unbind-key "C-'" org-mode-map)
  (unbind-key "C-c :" org-mode-map)
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-c \\" org-mode-map)
  (unbind-key "C-c a" org-mode-map)
  (unbind-key "C-c C-a" org-mode-map)
  (unbind-key "C-c C-j" org-mode-map)
  (unbind-key "C-c C-n" org-mode-map)
  (unbind-key "C-c C-p" org-mode-map)
  (unbind-key "C-c C-b" org-mode-map)

  (setq org-agenda-files '("~/Dropbox/org/TODO.org"
                           "~/Dropbox/org/birthdays.org"))

  (use-package ox-reveal
    :ensure t
    :config
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")))

(use-package org-agenda
  :bind (("C-c n" . org-agenda)
         ("C-c C-n" . org-agenda)
         :map org-mode-map
         ("C-c n" . org-agenda)
         ("C-c C-n" . org-agenda)
         ("C-c t" . org-timeline)
         ("C-c C-t" . org-timeline)))

(use-package htmlize
  :ensure t
  :defer 5)

(provide 'config-org)
