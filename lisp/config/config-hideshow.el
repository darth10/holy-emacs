;;; Configuration for hideshow code folding

(require 'hideshow)

(defun configure-hideshow ()
  (interactive)
  (define-key hs-minor-mode-map (kbd "C-c h") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c g h") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c s") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c g s") 'hs-show-all))

(add-hook 'hs-minor-mode-hook 'configure-hideshow)

(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'csharp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)

(provide 'config-hideshow)
