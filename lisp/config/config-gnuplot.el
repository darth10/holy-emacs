;;; Configuration for gnuplot

(require 'gnuplot)
(require 'gnuplot-mode)

(defconfig configure-gnuplot
  (auto-complete-mode)
  (god-local-mode t)
  (local-unset-key (kbd "C-c C-k"))
  (local-set-key (kbd "C-c C-k") 'gnuplot-send-buffer-to-gnuplot)
  (local-unset-key (kbd "C-c C-z"))
  (local-set-key (kbd "C-c C-z") 'gnuplot-show-gnuplot-buffer)
  (local-set-key (kbd "C-c k") 'gnuplot-kill-gnuplot-buffer)
  (local-set-key (kbd "C-<f5>") 'gnuplot-send-buffer-to-gnuplot)
  (local-set-key (kbd "C-?") 'gnuplot-info-at-point))

(add-hook 'gnuplot-mode-hook 'configure-gnuplot)

(provide 'config-gnuplot)
