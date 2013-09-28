;;; Configuration for higlighting comment annotations

(defun font-lock-comment-annotations ()
  "Highlight well known comment annotations"
  (let* ((delimiter "[^a-zA-Z0-9]")
         (annotation-regex
          (concat delimiter "\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)" delimiter)))
    (font-lock-add-keywords
     nil `((,annotation-regex 1 font-lock-warning-face t)))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'config-comment-annotations)
