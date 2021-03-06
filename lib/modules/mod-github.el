;;; mod-github.el --- Integration with GitHub        -*- lexical-binding: t; -*-

(use-package gist
  :defer 5)

(use-package github-review
  :after magit
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" github-review-forge-pr-at-point)))

(use-package forge
  :after magit
  :config
  (setq forge-database-file (concat core-var-cache-dir-full-path
                                    "forge-database.sqlite")
        ;; forge-pull-notifications fails for a large number of notifications
        forge-pull-notifications nil)
  (transient-append-suffix 'magit-branch "w"
    '("y" "pull request" forge-checkout-pullreq))
  (transient-append-suffix 'magit-branch "W"
    '("Y" "from pull request" forge-branch-pullreq)))

(provide 'mod-github)
