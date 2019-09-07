;;; core-keys.el --- holy-emacs core key bindings    -*- lexical-binding: t; -*-

;;; :lang extension keys

(defcustom holy-emacs-lang-repl-start-keys
  '("C-<f10>"
    "C-! r"
    "C-! C-r")
  "Keys to start a REPL for a language."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-repl-connect-keys
  '("C-<f8>"
    "C-! o"
    "C-! C-o")
  "Keys to connect to a REPL for a language."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-find-definition-keys
  '("<f12>"
    "M-.")
  "Keys to find definition."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-find-usages-keys
  '("S-<f12>"
    "C-c u")
  "Keys to find usages."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-eval-buffer-keys
  '("C-<f5>"
    "C-c a")
  "Keys to evaluate the current buffer."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-format-buffer-keys
  '("C-c f")
  "Keys to format the current buffer."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-load-file-keys
  '("C-c l")
  "Keys to load the file of the current buffer in a REPL."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-compile-file-keys
  '("C-c k")
  "Keys to compile the file of the current buffer."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-apply-refactor-keys
  '("C-c '")
  "Keys to apply refactoring."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-test-file-keys
  '("C-c t"
    "C-! t"
    "C-! C-t")
  "Keys to run tests in current buffer."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-test-all-keys
  '("C-c T"
    "C-! T"
    "C-! C-T")
  "Keys to run all tests in current project."
  :type '(repeat string)
  :group 'holy-emacs)

;;; debugger keys

(defcustom holy-emacs-lang-debugger-keys
  '("C-<f11>"
    "C-! d"
    "C-! C-d")
  "Keys to start a debugger for a language."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-set-breakpoint-keys
  '("<f9>"
    "C-x C-a C-b")
  "Keys to set a breakpoint for debugger."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-remove-breakpoint-keys
  '("S-<f9>"
    "C-x C-a C-d")
  "Keys to remove a breakpoint for debugger."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-step-over-keys
  '("<f10>"
    "C-x C-a C-n")
  "Keys to step over current line while debugging."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-step-into-keys
  '("<f11>"
    "C-x C-a C-s")
  "Keys to step into current line while debugging."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-step-out-keys
  '("S-<f11>"
    "C-x C-a C-f")
  "Keys to step out of current function while debugging."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-continue-keys
  '("<f5>"
    "C-x C-a C-c")
  "Keys to continue execution while debugging."
  :type '(repeat string)
  :group 'holy-emacs)

(defcustom holy-emacs-lang-debug-run-keys
  '("<f8>"
    "C-x C-a C-r")
  "Keys to continue execution until program termination while debugging."
  :type '(repeat string)
  :group 'holy-emacs)

(defun core-bind-keys (keys func &optional keymap predicate)
  "Binds multiple keys in the list KEYS to function FUNC.
An option KEYMAP and PREDICATE can also be specified for the
key binding."
  (cl-loop for key in keys
           collect key
           and do (eval `(bind-key ,key #',func ,keymap ,predicate))))

(provide 'core-keys)
