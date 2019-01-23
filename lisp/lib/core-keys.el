;;; Core key bindings -*- lexical-binding: t; -*-

(defconst core-lang-eval-buffer-keys
  '("C-<f5>"
    "C-x a a"
    "C-x C-a C-a"))

(defconst core-lang-repl-keys
  '("C-<f10>"
    "C-! r"
    "C-! C-r"))

(defun core-bind-keys (keys func &optional keymap predicate)
  "Binds multiple keys in the list KEYS to function FUNC.
An option KEYMAP and PREDICATE can also be specified for the
key binding."
  (cl-loop for key in keys
           collect key
           and do (eval `(bind-key ,key #',func ,keymap ,predicate))))

(provide 'core-keys)
