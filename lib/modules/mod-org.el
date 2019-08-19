;;; mod-org.el --- Configuration for org-mode        -*- lexical-binding: t; -*-

(use-package org
  :ensure t
  :pin org-elpa
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c b" . org-switchb)
         :map org-mode-map
         ("C-c n" . org-agenda)
         ("C-c C-n" . org-agenda)
         ("C-c l" . org-store-link)
         ("M-p" . org-metaup)
         ("M-n" . org-metadown))
  :init
  (setq org-preview-latex-image-directory
        (concat core-var-cache-dir-path "org-latex-images/"))
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

  (use-package ox-reveal
    :ensure t
    :config
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")))

(use-package org-agenda
  :bind (:map org-mode-map
         ("C-c n" . org-agenda)
         ("C-c C-n" . org-agenda)
         ("C-c t" . org-timeline)
         ("C-c C-t" . org-timeline)))

(use-package htmlize
  :ensure t
  :defer 5)

(use-package calendar
  :bind (("C-! c" . calendar)
         ("C-! C-c" . calendar)
         ("C-x <f11>" . calendar)))

(use-package calculator
  :bind (("C-! n" . calculator)
         ("C-! C-n" . calculator)
         ("C-x <f12>" . calculator))
  :config
  ;; increase size of calculator window
  (advice-add 'calculator :after #'(lambda () (enlarge-window 2))))

(provide 'mod-org)
