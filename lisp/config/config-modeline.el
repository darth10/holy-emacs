;;; Configuration for modeline

(add-to-list 'mode-line-format
             (quote (:eval (propertize (if sticky-control-mode " ^" "  ")
                                       'help-echo (concat "Sticky control mode " (if sticky-control-mode "enabled" "disabled"))))))

(add-to-list 'mode-line-format
             (quote (:eval (propertize (if god-local-mode "G" " ")
                                       'help-echo (concat "God mode " (if god-local-mode "enabled" "disabled"))))))

(provide 'config-modeline)
