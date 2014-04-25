;;; Configuration for search

(require 'ag)

;;; grep
(global-set-key (kbd "M-s G") 'grep)
(global-set-key (kbd "M-s g") 'rgrep)

;;; git grep
(global-set-key (kbd "C-: <f3>") 'vc-git-grep)
(global-set-key (kbd "M-s :") 'vc-git-grep)
(global-set-key (kbd "C-: M-s") 'vc-git-grep)

;;; ag
(global-set-key (kbd "C-<f3>") 'ag)
(global-set-key (kbd "M-s a a") 'ag)
(global-set-key (kbd "C-S-<f3>") 'ag-regexp)
(global-set-key (kbd "M-s a r") 'ag-regexp)
(global-set-key (kbd "M-s e a") 'ag-project)
(global-set-key (kbd "M-s e r") 'ag-project-regexp)

(provide 'config-search)
