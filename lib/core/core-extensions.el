;;; core-extensions.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides support for the `use-package' :lang extension/keyword.
;; This allows concise definitions of modules for editing, running
;; and debugging programming languages with consistent and
;; customizable key bindings.
;; The key definitions can be found in core/core-keys.el.
;;
;; Keywords:
;;     :map KEYMAP
;;     :filter SEXP
;;
;; Example:
;; (use-package python
;;   ;; ...
;;   :lang (:map python-mode-map
;;          (:eval-buffer . load-file-in-inf-python)
;;          (:repl-start . run-python)
;;          :filter (is-python-p))
;;   ;; ...
;;   )

(eval-and-compile
  (require 'core-keys)
  (require 'use-package-core)

  (add-to-list 'use-package-keywords :lang t))

(defconst core--lang-extension-keys-lookup-alist
  '((:repl-start         . core-lang-repl-start-keys)
    (:repl-connect       . core-lang-repl-connect-keys)
    (:find-definition    . core-lang-find-definition-keys)
    (:find-usages        . core-lang-find-usages-keys)
    (:eval-buffer        . core-lang-eval-buffer-keys)
    (:format-buffer      . core-lang-format-buffer-keys)
    (:load-file          . core-lang-load-file-keys)
    (:compile-file       . core-lang-compile-file-keys)
    (:test-file          . core-lang-test-file-keys)
    (:test-all           . core-lang-test-all-keys)
    (:debugger           . core-lang-debugger-keys)
    (:debug-set-break    . core-lang-debug-set-breakpoint-keys)
    (:debug-remove-break . core-lang-debug-remove-breakpoint-keys)
    (:debug-step-over    . core-lang-debug-step-over-keys)
    (:debug-step-into    . core-lang-debug-step-into-keys)
    (:debug-step-out     . core-lang-debug-step-out-keys)
    (:debug-continue     . core-lang-debug-continue-keys)
    (:debug-run          . core-lang-debug-run-keys)))

(defun core--bind-language-keys-form (args)
  "Internal function to generate `core-bind-keys' forms. Should be used
within a `use-package' handler definition."
  (let (map
        filter)

    (let ((cont t))
      (while (and cont args)
        (if (cond ((eq :map (car args))
                   (setq map (cadr args)))
                  ((eq :filter (car args))
                   (setq filter (cadr args)) t))
            (setq args (cddr args))
          (setq cont nil))))

    (let (first next)
      (while args
        (if (keywordp (car args))
            (progn
              (setq next args)
              (setq args nil))
          (if first
              (nconc first (list (car args)))
            (setq first (list (car args))))
          (setq args (cdr args))))

      (when (or (not map)
                (not first))
        (error ":lang :map requires forms"))

      (append
       (cl-mapcan
        (lambda (form)
          (let ((keys (cdr (assoc (car form)
                                  core--lang-extension-keys-lookup-alist)))
                (fun (and (cdr form) (list 'function (cdr form)))))
            (if (and map (not (eq map 'global-map)))
                `((defvar ,map (make-sparse-keymap))
                  (core-bind-keys ,keys ,fun ',map ,filter))
              `((core-bind-keys ,keys ,fun nil ,filter)))))
        first)
       (when next
         (core--bind-language-keys-form next))))))

(defun core--lang-extension-normalizer (name keyword args)
  "Normalize arguments for `use-package' :lang extension."
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ((and (consp x)
               (keywordp (car x))
               (assoc (car x) core--lang-extension-keys-lookup-alist)
               (or (use-package-recognize-function (cdr x) t #'stringp)))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; Keywords:
         ;;   :map KEYMAP
         ;;   :filter SEXP
         ((or (and (eq x :map) (symbolp (cadr arg)))
              (eq x :filter))
          (setq args* (nconc args* (list x (cadr arg))))
          (setq arg (cddr arg)))
         ((listp x)
          (setq args*
                (nconc args* (core--lang-extension-normalizer name keyword x)))
          (setq arg (cdr arg)))
         (t
          (use-package-error
           (concat (symbol-name name)
                   " :lang received bad values"))))))
    args*))

(defun core--lang-extension-handler (name _keyword args rest state)
  "Generate forms for `use-package' :lang extension."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(cl-mapcan
        (lambda (xs)
          (core--bind-language-keys-form
           (use-package-normalize-commands xs)))
        (use-package-split-list-at-keys :break args)))))

;;;###autoload
(defalias 'use-package-normalize/:lang 'core--lang-extension-normalizer)

;;;###autoload
(defalias 'use-package-handler/:lang 'core--lang-extension-handler)

(provide 'core-extensions)
