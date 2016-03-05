;;; Configuration for recentf, breadcrumbs

(use-package recentf
  :bind (("C-c : ;" . recentf-open-files)
         ("C-c C-: C-;" . recentf-open-files))
  :config
  (custom-set-variables
   '(recentf-max-menu-items 40)
   '(recentf-max-saved-items 40)))

(use-package breadcrumb
  :load-path "lisp/lib/"
  :bind (("C-c : #" . bc-clear-and-msg)
         ("C-c : :" . bc-list)
         ("C-c : <down>" . bc-local-next)
         ("C-c : <left>" . bc-previous)
         ("C-c : <right>" . bc-next)
         ("C-c : <up>" . bc-local-previous)
         ("C-c : =" . bc-set)
         ("C-c : b" . bc-previous)
         ("C-c : f" . bc-next)
         ("C-c : n" . bc-local-next)
         ("C-c : p" . bc-local-previous)
         ("C-c C-: C-#" . bc-clear-and-msg)
         ("C-c C-: C-:" . bc-list)
         ("C-c C-: C-<down>" . bc-local-next)
         ("C-c C-: C-<left>" . bc-previous)
         ("C-c C-: C-<right>" . bc-next)
         ("C-c C-: C-<up>" . bc-local-previous)
         ("C-c C-: C-=" . bc-set)
         ("C-c C-: C-b" . bc-previous)
         ("C-c C-: C-f" . bc-next)
         ("C-c C-: C-n" . bc-local-next)
         ("C-c C-: C-p" . bc-local-previous))
  :config
  (defun bc-clear-and-msg ()
    (interactive)
    (bc-clear)
    (message "All breadcrumbs deleted!")))

(provide 'config-bookmarks)
