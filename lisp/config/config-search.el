;;; Configuration for search

(require 'ag)

;;; occur
(global-set-key (kbd "M-s i") 'helm-occur)

;;; grep
(global-set-key (kbd "M-s G") 'grep)
(global-set-key (kbd "M-s g") 'rgrep)

;;; git grep
(global-set-key (kbd "C-: <f3>") 'vc-git-grep)
(global-set-key (kbd "M-s :") 'vc-git-grep)
(global-set-key (kbd "C-: M-s") 'vc-git-grep)

;;; ag
(global-set-key (kbd "C-<f3>") 'ag)
(global-set-key (kbd "M-s s") 'ag)
(global-set-key (kbd "C-S-<f3>") 'ag-regexp)
(global-set-key (kbd "M-s r") 'ag-regexp)
(global-set-key (kbd "M-s a s") 'ag-project)
(global-set-key (kbd "M-s a r") 'ag-project-regexp)

(provide 'config-search)
