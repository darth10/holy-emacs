;;; core-extensions.el --- holy-emacs extensions for use-package  -*- lexical-binding: t; -*-

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
;;     :comp (MODE . COMPANY-BACKEND)
;;
;; Example:
;; (use-package python
;;   ;; ...
;;   :lang (:comp (python-mode . company-jedi)
;;          :map python-mode-map
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
  (let (comp
        map
        filter)

    (let ((cont t))
      (while (and cont args)
        (if (cond ((eq :comp (car args))
                   (setq comp (cadr args)))
                  ((eq :map (car args))
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

      (when (and (not comp)
                 (or (not map)
                     (not first)))
        (error ":lang requires :map or :comp forms"))

      (append
       (when comp
         (let* ((mode (car comp))
                (mode-hook (intern (concat (symbol-name mode) "-hook")))
                (comp-backend (cdr comp)))
           `((when (boundp 'company-backends)
               (add-hook
                ',mode-hook
                #'(lambda ()
                    (setq-local company-backends
                                (cons ',comp-backend company-backends))))))))
       (when (and first map (not (eq map 'global-map)))
         `((defvar ,map (make-sparse-keymap))))
       (cl-mapcan
        (lambda (form)
          (let ((keys (cdr (assoc (car form)
                                  core--lang-extension-keys-lookup-alist)))
                (fun (and (cdr form) (list 'function (cdr form)))))
            (if (and map (not (eq map 'global-map)))
                `((core-bind-keys ,keys ,fun ',map ,filter))
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
         ;;   :comp (MODE . COMPANY-BACKEND)
         ((and (eq x :comp) (and (listp (cadr arg))
                                 (not (listp (cdadr arg)))))
          (setq args* (nconc args* (list x (cadr arg))))
          (setq arg (cddr arg)))
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
