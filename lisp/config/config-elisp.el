;;; Configuration for Emacs Lisp

(use-package lisp-mode
  :config
  (bind-key "C-x a a" 'eval-print-last-sexp lisp-interaction-mode-map)
  (bind-key "C-x C-a C-a" 'eval-print-last-sexp lisp-interaction-mode-map)
  (bind-key "C-<f5>" 'eval-print-last-sexp lisp-interaction-mode-map)
  (bind-key "C-c l" 'load-file emacs-lisp-mode-map)
  (bind-key "C-c C-l" 'load-file emacs-lisp-mode-map)
  (bind-key "C-c k" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-x a a" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-x C-a C-a" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-<f5>" 'eval-buffer emacs-lisp-mode-map))

(provide 'config-elisp)
