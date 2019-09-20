;;; mod-lang-elisp.el --- Configuration for Emacs Lisp  -*- lexical-binding: t; -*-

(use-package lisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . flycheck-mode)
         (emacs-lisp-mode . paredit-mode)
         (emacs-lisp-mode . +elisp--highlight-sexp-setup)
         (flycheck-mode . +elisp--flycheck-setup))
  :lang (:map emacs-lisp-mode-map
         (:repl-start . +elisp/find-or-run-eshell)
         (:eval-buffer . eval-buffer)
         (:load-file . load-file)
         (:compile-file . +elisp/compile-file)
         :map lisp-interaction-mode-map
         (:eval-buffer . eval-print-last-sexp))
  :bind (:map emacs-lisp-mode-map
         ("C-c M-m" . emacs-lisp-macroexpand))
  :init
  (setq flycheck-emacs-lisp-initialize-packages t
        flycheck-emacs-lisp-load-path 'inherit)

  (defun +elisp--highlight-sexp-setup ()
    (+highlight-sexp:bind-keys 'emacs-lisp-mode-map))

  (defun +elisp--flycheck-setup ()
    ;; Flycheck for emacs-lisp calls `byte-compile-file'.
    ;; Set `flycheck-emacs-lisp-package-initialize-form' here to:
    ;; 1. Avoid calling `package-initialize'.
    ;; 2. Partially load Emacs configuration (part of init.el) so that
    ;;    forms like `use-package' can be compiled.
    (setq flycheck-emacs-lisp-package-initialize-form
          (flycheck-sexp-to-string
           '(with-demoted-errors "Error during package initialization: %S"
              (require 'core (concat user-emacs-directory "lib/core/core"))
              (core:initialize-packages-and-modules)))))
  :config
  (defun +elisp/compile-file ()
	(interactive)
	(core:compile-file buffer-file-name)))

(use-package ielm
  :straight nil
  :hook (ielm-mode . paredit-mode)
  :bind (("C-! i" . ielm)
         ("C-! C-i" . ielm)))

(use-package eldoc
  :straight nil
  :hook (((emacs-lisp-mode ielm-mode eshell-mode) . eldoc-mode)
         (eval-expression-minibuffer-setup . +elisp--turn-off-eldoc))
  :config
  ;; Disable eldoc for `eval-expression'.
  (defun +elisp--turn-off-eldoc ()
    (eldoc-mode -1)))

(use-package eshell
  :straight nil
  :commands (+elisp/load-bash-aliases-in-eshell
             +elisp/find-or-run-eshell)
  :hook ((eshell-mode . +elisp/load-bash-aliases-in-eshell)
         (eshell-mode . paredit-mode))
  :bind (("C-! e" . +elisp/find-or-run-eshell)
         ("C-! C-e" . +elisp/find-or-run-eshell))
  :config
  (setq eshell-directory-name (concat core-var-cache-dir-full-path "eshell/"))

  (defun +elisp/find-or-run-eshell ()
    (interactive)
    (core:find-or-run-process
     "*eshell*"
     #'eshell))

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
      (if (get-buffer "bash-aliases") (kill-buffer "bash-aliases"))
      (if (get-buffer "bash-errors") (kill-buffer "bash-errors"))
      (message "Loaded aliases.")
      (delete-other-windows))))

(use-package eval-sexp-fu
  :hook ((lisp-mode emacs-lisp-mode eshell-mode) . +eval-sexp-fu--init)
  :custom-face
  (eval-sexp-fu-flash ((t (:background "green" :foreground "black"))))
  :config
  (defun +eval-sexp-fu--init ()
    (require 'eval-sexp-fu)))

(use-package esup
  :commands (esup))

(provide 'mod-lang-elisp)
