;;; mod-lang-elisp.el --- Configuration for Emacs Lisp  -*- lexical-binding: t; -*-

(use-package lisp-mode
  :lang (:map emacs-lisp-mode-map
         (:repl-start . core/find-or-run-eshell)
         (:eval-buffer . eval-buffer)
         (:load-file . load-file)
         (:compile-file . +elisp/compile-file)
         :map lisp-interaction-mode-map
         (:eval-buffer . eval-print-last-sexp))
  :config
  (defun +elisp/compile-file ()
	(interactive)
	(core:compile-file buffer-file-name)))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eshell-mode-hook 'eldoc-mode))

(use-package eshell
  :config
  (defun +elisp/load-bash-aliases-in-eshell ()
    "Reads bash aliases from Bash and inserts
    them into the list of eshell aliases."
    (interactive)
    (progn
      (shell-command "alias" "bash-aliases" "bash-errors")
      (switch-to-buffer "bash-aliases")
      (replace-string "alias " "")
      (goto-char 1)
      (replace-string "='" " ")
      (goto-char 1)
      (replace-string "'\n" "\n")
      (goto-char 1)
      (let ((alias-name) (command-string) (alias-list))
        (while (not (eobp))
          (while (not (char-equal (char-after) 32))
            (forward-char 1))
          (setq alias-name
                (buffer-substring-no-properties (line-beginning-position) (point)))
          (forward-char 1)
          (setq command-string
                (buffer-substring-no-properties (point) (line-end-position)))
          (setq alias-list (cons (list alias-name command-string) alias-list))
          (forward-line 1))
        (setq eshell-command-aliases-list alias-list))
      (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
      (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))
      (message "Loaded aliases.")))

  (add-hook 'eshell-mode-hook #'+elisp/load-bash-aliases-in-eshell))

(use-package eval-sexp-fu
  :ensure t
  :commands (+eval-sexp-fu--init)
  :init
  (add-hook 'lisp-mode-hook #'+eval-sexp-fu--init)
  (add-hook 'emacs-lisp-mode-hook #'+eval-sexp-fu--init)
  (add-hook 'eshell-mode-hook #'+eval-sexp-fu--init)
  :config
  (face-spec-set 'eval-sexp-fu-flash '((t (:background "green" :foreground "black"))))
  (defun +eval-sexp-fu--init ()
    (require 'eval-sexp-fu)))

(use-package esup
  :ensure t
  :commands (esup))

(use-package try
  :ensure t
  :commands (try))

(provide 'mod-lang-elisp)
