## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, C#, JavaScript, Haskell, Python, Ruby, PHP and C.
* Identical (almost) bindings across different major modes.
* Installs all required packages from GNU, Marmalade and MELPA repositories on startup.
* Upgrade all packages using `M-x util/upgrade`.
* Includes `helm`, `yasnippet`, `company` and `magit`.
* Uses `god-mode` for modal editing. All key bindings work in `god-mode`, and `god-mode` is only used to reduce the usage of modifier keys.
* Uses `use-package` and `bind-key`. `M-x describe-personal-keybindings` will display all available key bindings.
* Uses `which-key` to interactively describe key bindings.
* Uses `tramp` for editing remote files. `C-x C-f` (or `M-x find-file`) with paths like `/sshx:user@remotehost|sudo:remotehost:/etc/dhcpd.conf` will open remote files.
* Uses `ag` for file and project search.
* Uses `desktop-save-mode` to manage sessions and buffers.
* Automatically saves backup files to `~/.emacs.bak/`.
* Recompile entire `~/.emacs.d` using `M-x util/rebuild` or the `rebuild.sh` script.

## `god-mode`

Use <kbd>ESC</kbd> or <kbd>M-i</kbd> to toggle `god-mode`.
In `god-mode`, <kbd>i</kbd> will also disable `god-mode`.
Remember that you can use <kbd>C-[</kbd>, or just <kbd>[</kbd> in `god-mode`, instead of <kbd>ESC</kbd>.
This has the same effect as using the meta (<kbd>M-</kbd>) modifier.

* <kbd>C-x C-c</kbd> becomes <kbd>xc</kbd>.
* <kbd>C-x (</kbd> becomes <kbd>x (</kbd>. Note the use of the space key.
* <kbd>M-x</kbd> becomes <kbd>gx</kbd>.
* <kbd>C-M-c</kbd> becomes <kbd>Gc</kbd>.
* <kbd>M-10 C-n</kbd> becomes <kbd>10n</kbd>.
* <kbd>.</kbd> or <kbd>z</kbd> will repeat the last command.

## Overridden default key bindings

Some of the default Emacs key bindings are changed, as follows.

* <kbd>C-s</kbd> will save the current buffer, while <kbd>C-x C-s</kbd> will perform an incremental search.
* <kbd>C-z</kbd> doesn't minimize Emacs.
* <kbd>C-x a n</kbd> and <kbd>F10</kbd> don't have their usual behaviour, as they are used by GUD key bindings.
* <kbd>C-x C-c</kbd> will exit Emacs with a confirmation.
* <kbd>M-i</kbd> doesn't call `tab-to-tab-stop` and is used to toggle `god-mode`. Use <kbd>M-[</kbd> or <kbd>C-q TAB</kbd> instead.
* <kbd>M-SPC</kbd> doesn't call `just-one-space`, and is used for auto-completion. Use <kbd>C-c \\</kbd> or <kbd>C-c C-\\</kbd> instead.
* When no region is selected, <kbd>C-w</kbd> will kill the current line and <kbd>M-w</kbd> will copy the current line.

## Scripts

All scripts are in the `script/` folder.

| Script             | Description                                                                                 |
| ------------------ | ------------------------------------------------------------------------------------------- |
| rebuild.sh         | Recompile all Emacs Lisp files in `~/.emacs.d`.                                             |
| clean.sh           | Delete all compiled Emacs Lisp files in `~/.emacs.d`.                                       |
| fetch-info-docs.sh | Fetch info documentation. Available only on GNU/Linux.                                      |
| clean-ghc.sh       | Cleans GHC packages. Available if `ghc-pkg` and `cabal` are installed.                      |
