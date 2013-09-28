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
  (let ((wind-key (read-key "Select next window")))
    (cond ((eq wind-key 'left)  (windmove-left))
	  ((eq wind-key 'right) (windmove-right))
	  ((eq wind-key 'up)    (windmove-up))
	  ((eq wind-key 'down)  (windmove-down))
	  (t                    (message "Unknown window")))))

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

(defun split-and-term ()
  "Split window and start terminal"
  (interactive)
  (split-window-below -10)
  (other-window 1)
  (term "/bin/bash"))

(defun git-diff-tree ()
  (interactive)
  (magit-diff-working-tree "HEAD")
  (other-window 1))

(provide 'util)
