;;; core-editor.el --- holy-emacs core editor functions  -*- lexical-binding: t; -*-

(defconst core--scratch-buffer-name "*scratch*")

(defun core:find-or-run-process (new-buffer-name process-f)
  "Switches to or opens up a process using PROCESS-F in
a new buffer named NEW-BUFFER-NAME."
  (let* ((process-buffer (get-buffer new-buffer-name)))
    (if (eq (window-buffer) process-buffer)
        (delete-other-windows)
      (if (eq 1 (count-windows))
          (split-window-vertically))
      (if (not (eq (window-buffer) process-buffer))
          (other-window 1)))
    (or (and process-buffer
             (switch-to-buffer process-buffer))
        (funcall process-f))))

(defun core:move-line (n)
  "Move the current line up or down by N lines."
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun core:move-region (start end n)
  "Move the current region up or down by N lines."
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun core/kill-ring-save-line (beg end flash)
  "Copy current line to kill ring."
  (interactive (if (use-region-p)
           (list (region-beginning) (region-end) nil)
         (list (line-beginning-position)
               (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
      (goto-char end)
    (goto-char beg)))))

(defun core:kill-line-utils-init ()
  "Sets alternate kill/copy key bindings."
  ;; M-w
  (global-set-key [remap kill-ring-save] 'core/kill-ring-save-line)
  ;; C-w
  (put 'kill-region 'interactive-form
       '(interactive
         (if (use-region-p)
             (list (region-beginning) (region-end))
           (list (line-beginning-position) (line-beginning-position 2))))))

(defun core/confirm-and-kill-terminal ()
  "Quit Emacs with a confirmation."
  (interactive)
  (when (yes-or-no-p "Quit Emacs? ")
    (save-buffers-kill-terminal)))

(defun core/find-user-init-file ()
  "Edit user-init-file in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun core/match-paren (arg)
  "Go to the matching paren if the cursor is on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (message "%s" "No parenthesis under cursor!"))))

;;; Interactive window and switching functions

(defun core/switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer core--scratch-buffer-name))

(defun core/switch-to-scratch-other-window ()
  "Switch to *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window core--scratch-buffer-name))

(defun core/switch-to-scratch-other-frame ()
  "Switch to *scratch* buffer in a new frame."
  (interactive)
  (switch-to-buffer-other-frame core--scratch-buffer-name))

(defun core/resize-window (key)
  "Interactively resize the current window."
  (interactive "cUse {/} to resize vertically, or </> to resize horizontally")
  (cond
   ((eq key (string-to-char "{"))
    (enlarge-window 1)
    (call-interactively #'core/resize-window))
   ((eq key (string-to-char "}"))
    (enlarge-window -1)
    (call-interactively #'core/resize-window))
   ((eq key (string-to-char ">"))
    (enlarge-window-horizontally 1)
    (call-interactively #'core/resize-window))
   ((eq key (string-to-char "<"))
    (enlarge-window-horizontally -1)
    (call-interactively #'core/resize-window))
   (t (push key unread-command-events))))

(defun core/switch-to-window ()
  "Switches to a different window."
  (interactive)
  (let ((wind-key (read-key "Use f/b/n/p or cursor keys to move to next ")))
    (cond
     ((or (eq wind-key 'left)
          (eq wind-key ?b))    (windmove-left))
     ((or (eq wind-key 'right)
          (eq wind-key ?f))    (windmove-right))
     ((or (eq wind-key 'up)
          (eq wind-key ?p))    (windmove-up))
     ((or (eq wind-key 'down)
          (eq wind-key ?n))    (windmove-down))
     (t nil))))

(defun core/delete-single-window (&optional window)
  "Close window WINDOW and kill its buffer."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
      (delete-window (selected-window)))))

;;; Interactive process functions

(defun core/list-processes-and-switch ()
  "Show all processes in a new window."
  (interactive)
  (list-processes)
  (other-window 1))

(defun core/find-or-run-shell ()
  "Switches to or opens up a new shell."
  (interactive)
  (core:find-or-run-process
   "*shell*"
   (lambda () (shell))))

(defun core/find-or-run-eshell ()
  "Switches to or opens up a new eshell."
  (interactive)
  (core:find-or-run-process
   "*eshell*"
   (lambda () (eshell "new"))))

;;; Interactive line and region movement functions

(defun core/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (core:move-line (if (null n) -1 (- n))))

(defun core/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (core:move-line (if (null n) 1 n)))

(defun core/move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (core:move-region start end (if (null n) -1 (- n))))

(defun core/move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (core:move-region start end (if (null n) 1 n)))

(defun core/move-line-region-up (start end n)
  "Move the current region or line up by N lines."
  (interactive "r\np")
  (if (region-active-p)
      (core/move-region-up start end n)
    (core/move-line-up n)))

(defun core/move-line-region-down (start end n)
  "Move the current region or line down by N lines."
  (interactive "r\np")
  (if (region-active-p)
      (core/move-region-down start end n)
    (core/move-line-down n)))

(provide 'core-editor)
