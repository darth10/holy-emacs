;;; Configuration for GUD

(defconst config-key-gud-break1 "<f9>")
(defconst config-key-gud-break2 "C-x C-a C-b")

(defconst config-key-gud-remove1 "S-<f9>")
(defconst config-key-gud-remove2 "C-x C-a C-d")

(defconst config-key-gud-next1 "<f10>")
(defconst config-key-gud-next2 "C-x C-a C-n")

(defconst config-key-gud-step1 "<f11>")
(defconst config-key-gud-step2 "C-x C-a C-s")

(defconst config-key-gud-finish1 "S-<f11>")
(defconst config-key-gud-finish2 "C-x C-a C-f")

(defconst config-key-gud-cont1 "<f5>")
(defconst config-key-gud-cont2 "C-x C-a C-c")

(defconst config-key-gud-run1 "<f8>")
(defconst config-key-gud-run2 "C-x a r")

(use-package gud
  :defer 5
  :config
  (defun config-show-no-gud ()
    (message "No active debug buffer"))

  (defmacro global-gud-set-key (key fn)
    (let ((gud-fn (intern fn))
          (global-gud-fn (intern (concat "global-" fn)))
          (doc-string (format "Call (%s ARG) interactively.\nSee `%s' function for more information." fn fn)))
      `(progn
         (defun ,global-gud-fn (arg)
           ,doc-string
           (interactive "p")
           (if gud-minor-mode (,gud-fn arg)
             (config-show-no-gud)))
         (bind-key ,key ',global-gud-fn))))

  (setq gdb-many-windows t)
  ;; maybe use gud-def from gud.el
  (progn
    (global-gud-set-key config-key-gud-break1 "gud-break")
    (global-gud-set-key config-key-gud-break2 "gud-break")

    (global-gud-set-key config-key-gud-remove1 "gud-remove")
    (global-gud-set-key config-key-gud-remove2 "gud-remove")

    (global-gud-set-key config-key-gud-next1 "gud-next")
    (global-gud-set-key config-key-gud-next2 "gud-next")

    (global-gud-set-key config-key-gud-step1 "gud-step")
    (global-gud-set-key config-key-gud-step2 "gud-step")

    (global-gud-set-key config-key-gud-finish1 "gud-finish")
    (global-gud-set-key config-key-gud-finish2 "gud-finish")

    (global-gud-set-key config-key-gud-cont1 "gud-cont")
    (global-gud-set-key config-key-gud-cont2 "gud-cont")

    (global-gud-set-key config-key-gud-run1 "gud-run")
    (global-gud-set-key config-key-gud-run2 "gud-run")))

(provide 'mod-gud)
