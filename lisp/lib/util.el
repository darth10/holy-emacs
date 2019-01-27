;;; Some utility functions

(require 'cl)
(require 'cl-lib)

(defconst scratch-buffer-name "*scratch*")

(defun util/switch-to-scratch-other-frame ()
  "Switch to *scratch* buffer in a new frame."
  (interactive)
  (switch-to-buffer-other-frame scratch-buffer-name))

(defun util/switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer scratch-buffer-name))

(defun util/switch-to-scratch-other-window ()
  "Switch to *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window scratch-buffer-name))

(defun util/confirm-and-kill-terminal ()
  "Quit Emacs with a confirmation."
  (interactive)
  (when (yes-or-no-p "Quit Emacs? ")
    (save-buffers-kill-terminal)))

(defun util/find-user-init-file ()
  "Edit user-init-file in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun util/find-or-run-process (new-buffer-name process-f)
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

(defun util/list-processes-and-switch ()
  "Show all processes in a new window."
  (interactive)
  (list-processes)
  (other-window 1))

(defun util/resize-window (key)
  "Interactively resize the current window."
  (interactive "cUse {/} to resize vertically, or </> to resize horizontally")
  (cond
   ((eq key (string-to-char "{"))
    (enlarge-window 1)
    (call-interactively 'w-resize))
   ((eq key (string-to-char "}"))
    (enlarge-window -1)
    (call-interactively 'w-resize))
   ((eq key (string-to-char ">"))
    (enlarge-window-horizontally 1)
    (call-interactively 'w-resize))
   ((eq key (string-to-char "<"))
    (enlarge-window-horizontally -1)
    (call-interactively 'w-resize))
   (t (push key unread-command-events))))

(defun util/switch-to-window ()
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

(defun util/match-paren (arg)
  "Go to the matching paren if the cursor is on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (message "%s" "No parenthesis under cursor!"))))

(defun util/delete-single-window (&optional window)
  "Close window WINDOW and kill its buffer."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
      (delete-window (selected-window)))))

(defun util/kill-ring-save-line (beg end flash)
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

(defun util/kill-line-utils-init ()
  "Sets alternate kill/copy key bindings."
  ;; M-w
  (global-set-key [remap kill-ring-save] 'util/kill-ring-save-line)
  ;; C-w
  (put 'kill-region 'interactive-form
       '(interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2))))))

(defun util/find-or-run-shell ()
  "Switches to or opens up a new shell."
  (interactive)
  (util/find-or-run-process
   "*shell*"
   (lambda () (shell))))

(defun util/find-or-run-eshell ()
  "Switches to or opens up a new eshell."
  (interactive)
  (util/find-or-run-process
   "*eshell*"
   (lambda () (eshell "new"))))

(provide 'util)
