;;; Some utility functions

(require 'cl)
(require 'cl-lib)

(defconst scratch-buffer-name "*scratch*")

(defun switch-to-scratch-other-frame ()
  "Switch to *scratch* buffer in a new frame.\nCalls `switch-to-buffer-other-frame'."
  (interactive)
  (switch-to-buffer-other-frame scratch-buffer-name))

(defun switch-to-scratch ()
  "Switch to *scratch* buffer.\nCalls `switch-to-buffer'."
  (interactive)
  (switch-to-buffer scratch-buffer-name))

(defun switch-to-scratch-other-window ()
  "Switch to *scratch* buffer in a new window.\nCalls `switch-to-buffer-other-window'."
  (interactive)
  (switch-to-buffer-other-window scratch-buffer-name))

(defun confirm-and-kill-terminal ()
  "Quit Emacs with a confirmation."
  (interactive)
  (when (yes-or-no-p "Quit Emacs? ")
    (save-buffers-kill-terminal)))

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

(defun delete-single-window (&optional window)
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
      (delete-window (selected-window)))))

(defun split-and-eshell ()
  "Split window and opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (file-name-directory (buffer-file-name)))
         (name   (car
                  (last
                   (split-string parent "/" t)))))
    (split-window-below -10)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    ;; (when god-local-mode (god-local-mode -1)) ;; not sure about this bit
    (insert (concat "ls"))
    (eshell-send-input)))

(defun kill-ring-save-line (beg end flash)
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

(defun kill-line-utils-init ()
  ;; M-w
  (global-set-key [remap kill-ring-save] 'kill-ring-save-line)
  ;; C-w
  (put 'kill-region 'interactive-form
       '(interactive
	 (if (use-region-p)
	     (list (region-beginning) (region-end))
	   (list (line-beginning-position) (line-beginning-position 2))))))

(defun lvd-load-dir (d)
  (progn
    (add-to-list 'load-path d)
    (let* ((files (directory-files d))
	   (file-names (mapcar 'file-name-base files))
	   (dup-f (lambda (x y) (equal x y)))
	   (filter-f (lambda (x) (or (equal x ".")
				     (equal x ".gitignore"))))
	   (packages (remove-duplicates (cl-remove-if filter-f file-names)
					:test dup-f)))
      (mapcar 'load packages))
    (message (concat "Loaded all files from " d))))

(provide 'util)
