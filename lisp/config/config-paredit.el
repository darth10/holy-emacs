;;; Configuration for paredit-mode

(require 'paredit)

(defmacro set-paredit-key (key function)
  `(define-key paredit-mode-map (kbd ,key) (quote ,function)))

(defmacro unset-paredit-key (key)
  `(set-paredit-key ,key nil))

(eval-after-load "paredit"
  '(progn
     (unset-paredit-key "ESC <up>")
     (unset-paredit-key "M-<up>")
     (set-paredit-key "ESC M-<up>" paredit-splice-sexp-killing-backward)

     (unset-paredit-key "ESC <down>")
     (unset-paredit-key "M-<down>")
     (set-paredit-key "ESC M-<down>" paredit-splice-sexp-killing-forward)

     (unset-paredit-key "C-<right>")
     (set-paredit-key "M-<right>" paredit-forward-slurp-sexp)

     (unset-paredit-key "C-<left>")
     (set-paredit-key "M-<left>" paredit-forward-barf-sexp)

     (unset-paredit-key "ESC C-<right>")
     (set-paredit-key "ESC M-<right>" paredit-backward-barf-sexp)

     (unset-paredit-key "ESC C-<left>")
     (set-paredit-key "ESC M-<left>" paredit-backward-slurp-sexp)))

(provide 'config-paredit)
