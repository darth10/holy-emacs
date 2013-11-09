;;; Configuration for C#

(require 'csharp-mode)
(require 'config-common)

(defun omnisharp-intellisense ()
  (insert ".")
  (omnisharp-auto-complete))

(defun get-solution-file ()
  (let ((solution-file (read-file-name "Enter solution: ")))
    (if (equal system-type 'windows-nt)
	(replace-regexp-in-string "/" "\\\\" solution-file)
      solution-file)))

(defun get-omnisharp-server-command ()
  (let ((solution-file (get-solution-file)))
    (concat "Omnisharp -s \"" solution-file "\"")))

(defun start-omnisharp-server-process ()
  (start-process-shell-command
   "omnisharp-server"
   (generate-new-buffer-name "*omnisharp-server*")
   (get-omnisharp-server-command)))

(defun configure-omnisharp-bindings ()
  (local-set-key (kbd ".") 'omnisharp-intellisense)
  (local-set-key (kbd "C-x SPC") 'omnisharp-auto-complete)
  (local-set-key (kbd "C-?") 'omnisharp-show-overloads-at-point)
  (local-set-key (kbd "<f12>") 'omnisharp-go-to-definition)
  (local-set-key (kbd "S-<f12>") 'omnisharp-find-usages)
  (local-set-key (kbd "C-x <f5>") 'omnisharp-build-in-emacs))

(defun start-omnisharp ()
  (interactive)
  (start-omnisharp-server-process)
  (configure-omnisharp-bindings))

(defconfig configure-csharp
  (auto-complete-mode)
  (c-set-style "c#")
  (omnisharp-mode)
  (omnisharp-start-flycheck)
  (local-set-key (kbd "C-<f10>") 'start-omnisharp))

(add-hook 'csharp-mode-hook 'configure-csharp)

(provide 'config-csharp)
