;;; Configuration for Helm

(add-to-list 'load-path "~/.emacs.d/lisp.git/helm")

(require 'helm-config)
(require 'helm-ls-git)
(setq helm-split-window-default-side 'same)
(helm-mode)

(provide 'config-helm)
