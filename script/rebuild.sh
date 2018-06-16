#!/bin/bash

emacs --batch --eval \
'(progn
   (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)
   (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp/lib") 0)
   (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp/config") 0))'
