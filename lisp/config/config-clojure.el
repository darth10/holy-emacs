;;; Configuration for Clojure

(require 'config-common)

(require 'clojure-mode)
(require 'cider)
(require 'ac-cider)
(require 'clj-refactor)

(setq clj-add-ns-to-blank-clj-files nil)

(defun load-file-in-nrepl ()
  (interactive)
  (cider-load-buffer)
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-repl-buffer))

(defconfig configure-clojure-keys ()
  (local-set-key (kbd "C-<f10>") 'cider-jack-in)
  (local-set-key (kbd "C-! C-r") 'cider-jack-in)
  (local-set-key (kbd "C-<f5>") 'load-file-in-nrepl)
  (local-set-key (kbd "C-x C-a C-a") 'load-file-in-nrepl)
  (local-set-key (kbd "C-x a a") 'load-file-in-nrepl)
  (local-set-key (kbd "C-<f8>") 'cider-connect)
  (local-set-key (kbd "C-! C-o") 'cider-connect))

(defun configure-clojure ()
  (subword-mode +1)
  (clj-refactor-mode t)
  (local-unset-key (kbd "C-:"))
  (cljr-add-keybindings-with-prefix "C-c ESC"))

(defun configure-clojure-nrepl ()
  (paredit-mode)
  (helm-mode -1)
  (configure-clojure))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun configure-clojure-interaction ()
  (ac-flyspell-workaround)
  (ac-cider-setup )
  (local-set-key (kbd "C-?") 'cider-doc)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-popup-stacktraces t)
  (local-set-key (kbd "C-x T") 'cider-test-run-tests)
  (local-set-key (kbd "C-x t") 'cider-test-run-test))

(add-hook 'clojure-mode-hook 'configure-clojure)
(add-hook 'clojure-mode-hook 'configure-lisp)
(add-hook 'clojure-mode-hook 'configure-clojure-keys)

(add-hook 'cider-mode-hook 'configure-clojure-interaction)
(add-hook 'cider-repl-mode-hook 'configure-clojure-interaction)
(add-hook 'cider-repl-mode-hook 'configure-clojure-nrepl)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'config-clojure)
