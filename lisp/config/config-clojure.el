;;; Configuration for Clojure

(require 'config-common)
(require 'ac-nrepl)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-to-list 'same-window-buffer-names "*nrepl*")

(setq clojure-swank-command
      (if (or (locate-file "lein" exec-path) (locate-file "lein.bat" exec-path))
	  "lein ritz-in %s" "echo \"lein ritz-in %s\" | $SHELL -l"))

(defun split-and-cider-jack-in ()
  "Split window and start nREPL"
  (interactive)
  (split-window-right)
  (cider-jack-in nil))

(defun split-and-nrepl ()
  "Split window and start nREPL client"
  (interactive)
  (split-window-right)
  (cider "localhost"
	 (string-to-number (read-from-minibuffer "Port: "))))

(defun load-file-in-nrepl ()
  (interactive)
  (keyboard-escape-quit)
  (split-window-right)
  (cider-load-current-buffer)
  (cider-switch-to-repl-buffer (cider-current-ns)))

(defconfig configure-clojure-keys ()
  (local-set-key (kbd "C-<f10>") 'split-and-cider-jack-in)
  (local-set-key (kbd "C-<f5>") 'load-file-in-nrepl)
  (local-set-key (kbd "C-<f8>") 'split-and-nrepl))

(defun configure-clojure ()
  (subword-mode))

(defun configure-clojure-nrepl ()
  (paredit-mode)
  (ac-nrepl-setup)
  (configure-clojure)
  (clojure-test-mode))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun configure-clojure-nrepl-inf ()
  (ac-nrepl-setup)
  (local-set-key (kbd "C-?") 'cider-doc)
  (local-set-key (kbd "C-x T") 'clojure-test-run-tests)
  (local-set-key (kbd "C-x t") 'clojure-test-run-test))

(add-hook 'clojure-mode-hook 'configure-clojure)
(add-hook 'clojure-mode-hook 'configure-lisp)
(add-hook 'clojure-mode-hook 'configure-clojure-keys)
(add-hook 'cider-repl-mode-hook 'configure-clojure-nrepl)
(add-hook 'cider-mode-hook 'configure-clojure-nrepl-inf)

(provide 'config-clojure)
