;;; Configuration for C#

(require 'omnisharp)
(require 'config-common)

(defconst omnisharp-process-name
  "omnisharp-server")

(defun get-omnisharp-process-buffer-name ()
  (concat "*" omnisharp-process-name "*"))

(defun get-omnisharp-process-buffer ()
  (get-buffer (get-omnisharp-process-buffer-name)))

(defun omnisharp-intellisense ()
  (interactive)
  (insert ".")
  (when (get-omnisharp-process-buffer)
    (omnisharp-auto-complete)))

(defun get-solution-file ()
  (let ((solution-file (read-file-name "Enter solution: ")))
    (if (equal system-type 'windows-nt)
	(replace-regexp-in-string "/" "\\\\" solution-file)
      solution-file)))

(defun get-omnisharp-server-command ()
  (let ((solution-file (get-solution-file)))
    (concat "Omnisharp -s \"" solution-file "\"")))

(defun start-omnisharp-server ()
  (interactive)
  (start-process-shell-command
   omnisharp-process-name
   (generate-new-buffer-name
    (get-omnisharp-process-buffer-name))
   (get-omnisharp-server-command)))

(defun configure-omnisharp-bindings ()
  (local-set-key (kbd "C-<f10>") 'start-omnisharp-server)
  (local-set-key (kbd "C-! C-r") 'start-omnisharp-server)
  (local-set-key (kbd ".") 'omnisharp-intellisense)
  (local-set-key (kbd "M-SPC") 'omnisharp-auto-complete)
  (local-set-key (kbd "C-?") 'omnisharp-show-overloads-at-point)
  (local-set-key (kbd "<f12>") 'omnisharp-go-to-definition)
  (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
  (local-set-key (kbd "C-c F") 'omnisharp-code-format)
  (local-set-key (kbd "S-<f12>") 'omnisharp-find-usages)
  (local-set-key (kbd "ESC M-.") 'omnisharp-find-usages)
  (local-set-key (kbd "C-x <f5>") 'omnisharp-build-in-emacs)
  (local-set-key (kbd "C-! C-k") 'omnisharp-build-in-emacs))

(defconfig configure-csharp
  (require 'csharp-mode)
  (auto-complete-mode)
  (smartparens-mode)
  (c-set-style "c#")
  (omnisharp-mode)
  (omnisharp-start-flycheck)
  (configure-omnisharp-bindings))

(add-hook 'csharp-mode-hook 'configure-csharp)

(provide 'config-csharp)
