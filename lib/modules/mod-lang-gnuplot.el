;;; Configuration for gnuplot

(use-package gnuplot
  :ensure t
  :defer 5)

(use-package gnuplot-mode
  :ensure t
  :defer 5
  :bind (:map gnuplot-mode-map
         ("C-c C-k" . gnuplot-send-buffer-to-gnuplot)
         ("C-<f5>" . gnuplot-send-buffer-to-gnuplot)
         ("C-x C-a C-a" . gnuplot-send-buffer-to-gnuplot)
         ("C-x a a" . gnuplot-send-buffer-to-gnuplot)
         ("C-c C-z" . gnuplot-show-gnuplot-buffer)
         ("C-<f10>" . gnuplot-show-gnuplot-buffer)
         ("C-! C-r" . gnuplot-show-gnuplot-buffer)
         ("C-?" . gnuplot-info-at-point)))

(provide 'mod-lang-gnuplot)
