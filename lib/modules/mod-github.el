;;; Configuration for GitHub integration

(use-package gh
  :ensure t
  :defer 5)

(use-package gist
  :ensure t
  :defer 5)

(use-package github-review
  :ensure t
  :after magit
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" github-review-forge-pr-at-point)))

(use-package forge
  :ensure t
  :after magit
  :config
  (setq forge-database-file (concat core-var-cache-dir-full-path
                                    "forge-database.sqlite")
        ;; forge-pull-notifications fails for a large number of notifications
        forge-pull-notifications nil)
  (transient-append-suffix 'magit-branch "w"
    '("y" "pull request" forge-checkout-pullreq))
  (transient-append-suffix 'magit-branch "W"
    '("Y" "from pull request" forge-branch-pullreq))

  ;; code for showing branch refs
  (when-let (pos (cl-position 'forge-insert-topic-labels forge-topic-headers-hook))
    (setcdr (nthcdr (- pos 1) forge-topic-headers-hook)
            (push 'forge-insert-topic-refs (nthcdr pos forge-topic-headers-hook))))
  (cl-defun forge-insert-topic-refs
      (&optional (topic forge-buffer-topic))
    (when (forge-pullreq-p topic)
      (magit-insert-section (topic-refs)
        (let* ((repo-separator (propertize ":" 'face 'magit-dimmed))
               (refs-separator (propertize "..." 'face 'magit-dimmed))
               (deleted (propertize "(deleted)" 'face 'magit-dimmed))
               (cross-repo-p (oref topic cross-repo-p))
               (base-repo (oref topic base-repo))
               (base-ref (oref topic base-ref))
               (head-repo (oref topic head-repo))
               (head-ref (oref topic head-ref))
               (full-base-ref (if cross-repo-p
                                  (concat base-repo repo-separator base-ref)
                                base-ref))
               ;; Need to check if head repo or ref are deleted.
               (full-head-ref (if cross-repo-p
                                  (if (and head-repo head-ref)
                                      (concat head-repo repo-separator head-ref)
                                    deleted)
                                (or head-ref deleted))))
          (insert (format "%-11s" "Refs: ")
                  full-base-ref
                  refs-separator
                  full-head-ref
                  "\n"))))))

(provide 'mod-github)
