(add-to-list 'load-path "~/.emacs.d/lisp/config/")

(require 'config-pkg)

(use-package diminish :ensure t)
(require 'config-common)

;;; before language configurations
(require 'config-company)
(require 'config-ediff)
(require 'config-gud)
(require 'config-helm)
(require 'config-magit)
(require 'config-utils)

;;; language configurations
(require 'config-c)
(require 'config-clojure)
(require 'config-csharp)
(require 'config-elisp)
(require 'config-gnuplot)
(require 'config-haskell)
(require 'config-java)
(require 'config-js)
(require 'config-org)
(require 'config-php)
(require 'config-python)
(require 'config-ruby)
(require 'config-scala)
(require 'config-scheme)
(require 'config-sql)

;;; after language configurations
(require 'config-lisps)
(require 'config-modes)
(require 'config-ui)
(require 'config-web)

;; Linux-only config
(unless (is-windows?)
  (require 'config-c)
  (eval-after-load 'info
    '(progn
       (push "/opt/local/share/info" Info-default-directory-list)
       (push "~/.emacs.d/info" Info-default-directory-list)))
  (push "/usr/bin/" exec-path))

;; Windows-only config
(when (is-windows?)
  (setq w32-get-true-file-attributes nil)
  (w32-send-sys-command 61488))

(global-set-key (kbd "<apps>") 'execute-extended-command)
(global-set-key (kbd "C-! C-c") 'calendar)
(global-set-key (kbd "C-! C-e") 'split-and-eshell)
(global-set-key (kbd "C-! C-k") 'compile)
(global-set-key (kbd "C-! C-n") 'calculator)
(global-set-key (kbd "C-! C-p") 'list-processes-and-switch)
(global-set-key (kbd "C-' ' q") 'auto-fill-mode)
(global-set-key (kbd "C-' C-' C-q") 'auto-fill-mode)
(global-set-key (kbd "C-' C-n") 'linum-mode)
(global-set-key (kbd "C-' C-w") 'toggle-truncate-lines)
(global-set-key (kbd "C-' n") 'linum-mode)
(global-set-key (kbd "C-' w") 'toggle-truncate-lines)
(global-set-key (kbd "C-+") 'w-resize)

(global-set-key (kbd "C-<f6>") 'linum-mode)
(global-set-key (kbd "C-<f9>") 'toggle-truncate-lines)

(global-set-key (kbd "C-`") 'sticky-control-mode)
(global-set-key (kbd "C-c C-\\") 'just-one-space)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c C-n") 'org-agenda)
(global-set-key (kbd "C-c \\") 'just-one-space)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c n") 'org-agenda)

(global-set-key (kbd "C-x '") 'switch-to-scratch)
(global-set-key (kbd "C-x 9") 'delete-single-window)
(global-set-key (kbd "C-x <C-M-return>") 'find-user-init-file)
(global-set-key (kbd "C-x <f11>") 'calendar)
(global-set-key (kbd "C-x <f12>") 'calculator)
(global-set-key (kbd "C-x <f3>") 'list-processes-and-switch)
(global-set-key (kbd "C-x <f5>") 'compile)
(global-set-key (kbd "C-x <f7>") 'split-and-eshell)
(global-set-key (kbd "C-x C-'") 'switch-to-scratch-other-window)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-5 C-0") 'delete-frame)
(global-set-key (kbd "C-x C-5 C-1") 'delete-other-frames)
(global-set-key (kbd "C-x C-5 C-2") 'make-frame-command)
(global-set-key (kbd "C-x C-9") 'delete-single-window)
(global-set-key (kbd "C-x C-a C-k") 'recompile)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x M-[") 'previous-buffer)
(global-set-key (kbd "C-x M-]") 'next-buffer)
(global-set-key (kbd "C-x \"") 'switch-to-scratch-other-frame)
(global-set-key (kbd "C-x a k") 'recompile)

(global-set-key (kbd "C-x |") 'find-user-init-file)
(global-set-key (kbd "C-|") 'move-to-window)
(global-set-key (kbd "M-<f5>") 'recompile)

(defconst backup-dir "~/.emacs-saves/")

(custom-set-variables
 '(auto-save-file-name-transforms `((".*" ,backup-dir t)))
 '(auto-save-list-file-prefix backup-dir)
 '(backup-directory-alist `((".*" . ,backup-dir)))
 '(create-lockfiles nil)
 '(js-auto-indent-flag t)
 '(js-indent-level 2)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/TODO.org"
     "~/Dropbox/org/code.org"
     "~/Dropbox/org/birthdays.org"
     "~/Dropbox/org/M-clj.org"))))

;;; server process
(use-package server
  :ensure t
  :config
  (server-start))

;;; edit-server process
(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

;;; enable these modes before loading saved desktop
(column-number-mode 1)
(desktop-save-mode 1)
