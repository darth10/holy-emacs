;;; mod-lang-gnuplot.el --- Configuration for gnuplot  -*- lexical-binding: t; -*-

(use-package gnuplot
  :ensure t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :lang (:map gnuplot-mode-map
         (:repl-start . gnuplot-show-gnuplot-buffer)
         (:eval-buffer . gnuplot-send-buffer-to-gnuplot))
  :config
  (add-hook 'gnuplot-mode-hook #'gnuplot-inline-display-mode))

(use-package gnuplot-mode
  :ensure t
  :after gnuplot)

(provide 'mod-lang-gnuplot)
