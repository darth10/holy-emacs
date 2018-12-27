EMACS=emacs -batch --eval '(setq user-emacs-directory default-directory)' -l init.el

all: install compile

i: install
c: compile

install: upgrade
compile: recompile

upgrade:
	@$(EMACS) -f core/upgrade-packages

recompile:
	@$(EMACS) -f core/byte-recompile-files

autoremove:
	@$(EMACS) -f core/autoremove-packages

clean:
	@$(EMACS) -f core/clean-byte-compiled-files

clean-ghc: SHELL := /bin/bash
clean-ghc:
	for p in `ghc-pkg check 2>&1  | grep problems | awk '{print $$6}' | sed -e 's/:$$//'` ; \
	do \
		echo unregistering $p ; \
		ghc-pkg unregister $p ; \
	done
	echo 'erasing directories under ~/.ghc'; rm -rf `find ~/.ghc -maxdepth 1 -type d`
	echo 'erasing ~/.cabal/lib'; rm -rf ~/.cabal/lib
	echo 'erasing ~/.cabal/packages'; rm -rf ~/.cabal/packages
	echo 'erasing ~/.cabal/share'; rm -rf ~/.cabal/share
