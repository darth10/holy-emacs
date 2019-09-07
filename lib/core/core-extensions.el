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
  '((:repl-start         . holy-emacs-lang-repl-start-keys)
    (:repl-connect       . holy-emacs-lang-repl-connect-keys)
    (:find-definition    . holy-emacs-lang-find-definition-keys)
    (:find-usages        . holy-emacs-lang-find-usages-keys)
    (:eval-buffer        . holy-emacs-lang-eval-buffer-keys)
    (:format-buffer      . holy-emacs-lang-format-buffer-keys)
    (:load-file          . holy-emacs-lang-load-file-keys)
    (:compile-file       . holy-emacs-lang-compile-file-keys)
    (:apply-refactor     . holy-emacs-lang-apply-refactor-keys)
    (:test-file          . holy-emacs-lang-test-file-keys)
    (:test-all           . holy-emacs-lang-test-all-keys)
    (:debugger           . holy-emacs-lang-debugger-keys)
    (:debug-set-break    . holy-emacs-lang-debug-set-breakpoint-keys)
    (:debug-remove-break . holy-emacs-lang-debug-remove-breakpoint-keys)
    (:debug-step-over    . holy-emacs-lang-debug-step-over-keys)
    (:debug-step-into    . holy-emacs-lang-debug-step-into-keys)
    (:debug-step-out     . holy-emacs-lang-debug-step-out-keys)
    (:debug-continue     . holy-emacs-lang-debug-continue-keys)
    (:debug-run          . holy-emacs-lang-debug-run-keys)))

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
                (mode-hook-fn (intern (format "core-lang-company-setup-%s"
                                              mode)))
                (comp-backend (cdr comp)))
           `((defalias ',mode-hook-fn
               (lambda ()
                 (when (boundp 'company-backends)
                   (setq-local company-backends '(,comp-backend)))))
             (add-hook ',mode-hook #',mode-hook-fn))))
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
