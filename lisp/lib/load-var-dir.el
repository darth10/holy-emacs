;;; load-var-dir.el - Loads all files from a directory

(require 'cl)
(require 'cl-lib)

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

(provide 'load-var-dir)
