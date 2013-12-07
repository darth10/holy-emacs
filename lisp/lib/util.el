;;; Some utility functions

(require 'breadcrumb)

(defun bc-clear-and-msg ()
  (interactive)
  (bc-clear)
  (message "All breadcrumbs deleted!"))

(defun find-user-init-file ()
  "Edit the user-init-file, in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun list-processes-and-switch ()
  (interactive)
  (list-processes)
  (other-window 1))

(defun w-resize (key)
  "Interactively resize the window"
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

(defun move-to-window ()
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

(defun match-paren (arg)
  "Go to the matching paren if the cursor is on a paren"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (message "%s" "No parenthesis under cursor!"))))

(defun rebuild ()
  "Recompile everything in ~/.emacs.d"
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun split-and-eshell ()
  "Split window and start terminal"
  (interactive)
  (split-window-below -10)
  (other-window 1)
  (eshell))

(provide 'util)
