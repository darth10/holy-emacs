;;; Core extensions -*- lexical-binding: t; -*-

;;
;; DESIGN:
;;
;; KEYWORD
;;   :eval-buffer FUNC
;;   :format-buffer FUNC
;;   :compile-file FUNC
;;   :load-file FUNC
;;   :go-to-definition
;;   :find-usages
;;
;;   :repl
;;   :connect-to-repl
;;   :debugger

(eval-and-compile
  (require 'core-keys)
  (require 'use-package-core)

  (add-to-list 'use-package-keywords :lang t))

(defconst core-lang-extension-keys-lookup
  '((:eval-buffer    . core-lang-eval-buffer-keys)
    (:repl           . core-lang-repl-keys)))

(defun core-lang-extension-normalizer (name keyword args)
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ((and (consp x)
               (keywordp (car x))
               (assoc (car x) core-lang-extension-keys-lookup)
               (or (use-package-recognize-function (cdr x) t #'stringp)))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; KEYWORD
         ;;   :map KEYMAP
         ;;   :filter SEXP
         ((or (and (eq x :map) (symbolp (cadr arg)))
              (eq x :filter))
          (setq args* (nconc args* (list x (cadr arg))))
          (setq arg (cddr arg)))
         ((listp x)
          (setq args*
                (nconc args* (core-lang-extension-normalizer name keyword x)))
          (setq arg (cdr arg)))
         (t
          ;; Error!
          (use-package-error
           (concat (symbol-name name)
                   " :language received bad values"))))))
    args*))

(defun core-lang-extension-handler
    (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(cl-mapcan
        (lambda (xs)
          (core-bind-language-keys-form
           (use-package-normalize-commands xs)))
        (use-package-split-list-at-keys :break args)))))

;;;###autoload
(defalias 'use-package-normalize/:lang 'core-lang-extension-normalizer)

;;;###autoload
(defalias 'use-package-handler/:lang 'core-lang-extension-handler)

(defun core-bind-language-keys-form (args)
  (let (map
        filter)

    ;; Process any initial keyword arguments
    (let ((cont t))
      (while (and cont args)
        (if (cond ((eq :map (car args))
                   (setq map (cadr args)))
                  ((eq :filter (car args))
                   (setq filter (cadr args)) t))
            (setq args (cddr args))
          (setq cont nil))))

    ;; Process key binding arguments
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
                                  core-lang-extension-keys-lookup)))
                (fun (and (cdr form) (list 'function (cdr form)))))
            (if (and map (not (eq map 'global-map)))
                `((core-bind-keys ,keys ,fun ',map ,filter))
              `((core-bind-keys ,keys ,fun nil ,filter)))))
        first)
       (when next
         (core-bind-language-keys-form next))))))

(provide 'core-extensions)
