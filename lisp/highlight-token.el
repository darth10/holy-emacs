;;; Interactive identical token highlighting

(defun hlt-interactive-search (key)
  (interactive "cUse [/] for previous/next token, ESC to unhighlight")
  (cond
   ((eq key (string-to-char "["))
    (hlt-previous-highlight)
    (call-interactively 'hlt-interactive-search))
   ((eq key (string-to-char "]"))
    (hlt-next-highlight)
    (call-interactively 'hlt-interactive-search))
   ((eq key 27)                  ;; escape key
    (hlt-unhighlight-all-prop t)
    (push key unread-command-events))
   (t
    (push key unread-command-events))))

(defun hlt-highlight-current-word ()
  (interactive)
  (let ((var-name (current-word t)))
    (when var-name
      (save-excursion
	(hlt-highlight-regexp-region
	 (point-min)
	 (point-max)
	 (regexp-quote var-name)))
      (call-interactively 'hlt-interactive-search))))

(provide 'highlight-token)
