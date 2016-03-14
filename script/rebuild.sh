#!/bin/bash

emacs --batch --eval '(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)'

