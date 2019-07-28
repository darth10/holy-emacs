EMACS_BATCH=emacs -batch --eval '(setq user-emacs-directory default-directory)'
EMACS_FULL=@$(EMACS_BATCH) -l init.el
EMACS_CORE=@$(EMACS_BATCH) -l lib/core/core.el
CUSTOM_DEFS_FILE=var/lib/custom-defs.el
CACHE_DIR=var/cache

all: install compile

i: install
c: compile

install: upgrade
compile: recompile

init-files:
	if [ ! -f $(CUSTOM_DEFS_FILE) ]; then touch $(CUSTOM_DEFS_FILE); fi
	if [ ! -d $(CACHE_DIR) ]; then mkdir $(CACHE_DIR); fi

upgrade: init-files
	@$(EMACS_FULL) -f core/upgrade-packages

recompile:
	@$(EMACS_FULL) -f core/byte-recompile-files

autoremove:
	@$(EMACS_FULL) -f core/autoremove-packages

clean:
	@$(EMACS_CORE) -f core/clean-byte-compiled-files

.PHONY: all init-files compile recompile install upgrade clean
