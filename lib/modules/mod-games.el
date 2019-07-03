;;; mod-games.el --- Games for Emacs                 -*- lexical-binding: t; -*-

(use-package chess
  :ensure t
  :commands (chess)
  :config
  (setq chess-default-display '(chess-ics1 chess-plain)
        chess-default-engine '(chess-stockfish
                               chess-gnuchess
                               chess-crafty
                               chess-glaurung
                               chess-fruit
                               chess-phalanx
                               chess-ai))
  (custom-set-faces
   '(chess-display-white-face ((t (:foreground "light gray"))))
   '(chess-display-black-face ((t (:background "gray8" :foreground "gold3"))))
   '(chess-ics1-white-face ((t (:foreground "light gray"))))
   '(chess-ics1-black-face ((t (:background "gray8" :foreground "gold3"))))
   '(chess-ics1-highlight-face ((t (:background "chartreuse"))))))

(provide 'mod-games)
