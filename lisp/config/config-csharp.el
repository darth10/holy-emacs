;;; Configuration for C#

(use-package csharp-mode
  :ensure t
  :mode (("\\.cs\\'" . csharp-mode))
  :config

  (use-package omnisharp
    :ensure t
    :bind (:map omnisharp-mode-map
           ("M-SPC" . company-omnisharp)
           ("." . company-omnisharp-intellisense)
           ("C-x <f5>" . omnisharp-build-in-emacs)
           ("C-! C-k" . omnisharp-build-in-emacs)
           ("M-." . omnisharp-go-to-definition)
           ("<f12>" . omnisharp-go-to-definition)
           ("S-<f12>" . omnisharp-find-usages)
           ("C-c f" . omnisharp-code-format))

    :config
    (setq omnisharp-server-executable-path "~/bin/omnisharp/OmniSharp.exe")

    (defun company-omnisharp-intellisense ()
      (interactive)
      (insert ".")
      (when omnisharp-mode
        (call-interactively 'company-omnisharp))))

  (defun get-omnisharp-process-name (solution-file)
    (concat "omnisharp-server:" solution-file))

  (defun get-omnisharp-process-buffer-name (solution-file)
    (concat "*OmniSharp:" solution-file "*"))

  (defun get-omnisharp-process-buffer ()
    (get-buffer (get-omnisharp-process-buffer-name)))

  (defun get-solution-file ()
    (let ((solution-file (read-file-name "Enter solution: ")))
      (if (equal system-type 'windows-nt)
          (replace-regexp-in-string "/" "\\\\" solution-file)
        solution-file)))

  (defun get-omnisharp-server-command (solution-file)
    (concat omnisharp-server-executable-path " -s \"" solution-file "\""))

  (defun start-omnisharp-server ()
    (interactive)
    (require 'omnisharp)
    (let* ((solution-file (get-solution-file))
           (new-process-name (get-omnisharp-process-name solution-file))
           (new-buffer-name (generate-new-buffer-name
                             (get-omnisharp-process-buffer-name solution-file)))
           (new-server-command (get-omnisharp-server-command solution-file)))
      (progn
        (start-process-shell-command
         new-process-name
         new-buffer-name
         new-server-command)
        (omnisharp-mode t))))

  (bind-key "C-<f10>" 'start-omnisharp-server csharp-mode-map)
  (bind-key "C-! C-r" 'start-omnisharp-server csharp-mode-map))

(provide 'config-csharp)
