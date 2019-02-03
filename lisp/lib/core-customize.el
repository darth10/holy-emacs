;;; core-customize.el -*- lexical-binding: t; -*-

(defconst core-custom-defs-file-path
  (concat user-emacs-directory core-var-dir-path "custom-defs.el")
  "Absolute path to save customize definitions.")

(defun core--before-customize-save-variable
    (variable value &optional _comment)
  "Internal function added as advice before `customize-save-variable`.
This is intended to be called indirectly by `customize-themes` when
the state is saved after modifying the currently enabled theme."
  (when (or (eq variable 'core-enabled-custom-themes)
            (eq variable 'custom-enabled-themes))
    (custom-set-variables
     `(core-enabled-custom-themes (quote ,value))
     `(custom-enabled-themes (quote ,value)))))

(defun core--before-custom-save-all ()
  "Internal function added as advice before `custom-save-all`.
This is intented to be called when `customize-group` changes values
for the `holy-emacs` group and saves the resulting state."
  (when (not (eq core-enabled-custom-themes custom-enabled-themes))
    (custom-set-variables
     `(custom-enabled-themes (quote ,core-enabled-custom-themes)))))

(use-package cus-edit
  :bind (:map custom-mode-map
         ("C-s" . Custom-save)))

(use-package cus-theme
  :bind (:map custom-new-theme-mode-map
         ("C-s" . custom-theme-save)
         :map custom-theme-choose-mode-map
         ("C-s" . custom-theme-save)))

(progn
  (setq custom-file core-custom-defs-file-path)
  (cl-flet
      ((recompile-custom (&rest _args)
                         (core:compile-file core-custom-defs-file-path)))
    (advice-add 'customize-save-variable :after #'recompile-custom)
    (advice-add 'custom-save-all :after #'recompile-custom))
  (advice-add 'customize-save-variable :before #'core--before-customize-save-variable)
  (advice-add 'custom-save-all :before #'core--before-custom-save-all))

(provide 'core-customize)
