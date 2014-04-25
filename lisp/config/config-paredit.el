;;; Configuration for paredit-mode

(require 'paredit)

(defmacro set-paredit-key (key function)
  `(define-key paredit-mode-map (kbd ,key) (quote ,function)))

(defmacro unset-paredit-key (key)
  `(set-paredit-key ,key nil))

(eval-after-load "paredit"
  '(progn
     (unset-paredit-key "M-s")

     (unset-paredit-key "ESC <up>")
     (unset-paredit-key "M-<up>")
     (set-paredit-key "ESC M-<up>" paredit-splice-sexp-killing-backward)
     (set-paredit-key "ESC ESC <up>" paredit-splice-sexp-killing-backward)
     (set-paredit-key "C-, C-, C-k" paredit-splice-sexp-killing-backward)

     (unset-paredit-key "ESC <down>")
     (unset-paredit-key "M-<down>")
     (set-paredit-key "ESC M-<down>" paredit-splice-sexp-killing-forward)
     (set-paredit-key "ESC ESC <down>" paredit-splice-sexp-killing-forward)
     (set-paredit-key "C-, C-k" paredit-splice-sexp-killing-forward)

     (unset-paredit-key "C-<right>")
     (set-paredit-key "M-<right>" paredit-forward-slurp-sexp)
     (set-paredit-key "ESC <right>" paredit-forward-slurp-sexp)
     (set-paredit-key "C-, C-f" paredit-forward-slurp-sexp)

     (unset-paredit-key "C-<left>")
     (set-paredit-key "M-<left>" paredit-forward-barf-sexp)
     (set-paredit-key "ESC <left>" paredit-forward-barf-sexp)
     (set-paredit-key "C-, C-b" paredit-forward-barf-sexp)

     (unset-paredit-key "ESC C-<right>")
     (set-paredit-key "ESC M-<right>" paredit-backward-barf-sexp)
     (set-paredit-key "ESC ESC <right>" paredit-backward-barf-sexp)
     (set-paredit-key "C-, C-, C-f" paredit-backward-barf-sexp)

     (unset-paredit-key "ESC C-<left>")
     (set-paredit-key "ESC M-<left>" paredit-backward-slurp-sexp)
     (set-paredit-key "ESC ESC <left>" paredit-backward-slurp-sexp)
     (set-paredit-key "C-, C-, C-b" paredit-backward-slurp-sexp)))

(provide 'config-paredit)
