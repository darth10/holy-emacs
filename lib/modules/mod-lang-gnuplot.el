;;; mod-lang-gnuplot.el --- Configuration for gnuplot  -*- lexical-binding: t; -*-

(use-package gnuplot
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :hook (gnuplot-mode . gnuplot-inline-display-mode)
  :lang (:map gnuplot-mode-map
         (:repl-start . gnuplot-show-gnuplot-buffer)
         (:eval-buffer . gnuplot-send-buffer-to-gnuplot)))

(provide 'mod-lang-gnuplot)
