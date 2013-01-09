#!/bin/bash

mkdir info
cd info
echo "Fetching GNU libc manual..."
wget http://www.gnu.org/software/libc/manual/info/libc-info.tar.gz
echo "Fetching GNU Emacs Lisp manual..."
wget http://www.gnu.org/software/emacs/manual/info/elisp.info.tar.gz
echo "Installing GNU libc manual..."
tar xvf libc-info.tar.gz
ginstall-info libc.info dir-libc
echo "Installing GNU Emacs Lisp manual..."
tar xvf elisp.info.tar.gz
ginstall-info elisp dir-elisp
echo "Cleaning up..."
rm libc-info.tar.gz
rm elisp.info.tar.gz
