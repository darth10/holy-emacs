## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, C#, JavaScript, Haskell, Python, Ruby, PHP and C.
* Uses `god-mode` to reduce keystrokes. All normal key bindings work in `god-mode` too; `god-mode` is only used to minimize the number of keys pressed.
* Uses `which-key` to interactively describe key bindings.
* Uses `use-package` and `bind-key`. `M-x describe-personal-keybindings` will display all available key bindings.
* Identical (almost) bindings across different major modes.
* Installs all required packages from GNU, Marmalade and MELPA repositories on startup.
* Includes `helm`, `yasnippet` and `company`.
* Git integration via `magit`.
* File and project search via `ag`.
* Manage sessions and buffers via `desktop-save-mode`.
* Automatically saves backup files to `~/.emacs.bak/`.
* Recompile entire `~/.emacs.d` using `M-x rebuild`.
* Remember that the meta (<kbd>M-</kbd>) modifier is the same as the <kbd>C-[</kbd> prefix.

## `god-mode`

Use <kbd>ESC</kbd> or <kbd>M-i</kbd> to toggle `god-mode`. 
In `god-mode`, <kbd>i</kbd> will also disable `god-mode`.
Note that you can still use <kbd>C-[</kbd> instead of the escape key.
In `god-mode`, you can simply use <kbd>[</kbd> for the escape key.

* <kbd>C-x C-c</kbd> becomes <kbd>xc</kbd>.
* <kbd>C-x (</kbd> becomes <kbd>x (</kbd>. Note the use of the space key.
* <kbd>M-x</kbd> becomes <kbd>gx</kbd>.
* <kbd>C-M-c</kbd> becomes <kbd>Gc</kbd>.
* <kbd>M-10 C-n</kbd> becomes <kbd>10n</kbd>.
* Use <kbd>.</kbd> or <kbd>z</kbd> to repeat the last command.

## Overridden default key bindings

Some of the default Emacs key bindings are changed, as follows.

* <kbd>C-s</kbd> will save the current buffer, while <kbd>C-x C-s</kbd> will perform an incremental search.
* <kbd>C-z</kbd> doesn't minimize Emacs.
* <kbd>C-x a n</kbd> and <kbd>F10</kbd> don't have their usual behaviour, as they are used by GUD key bindings.
* <kbd>C-x C-c</kbd> will exit Emacs with a confirmation.
* <kbd>M-i</kbd> doesn't call `tab-to-tab-stop` and is used to toggle `god-mode`. Use <kbd>C-c TAB</kbd> or <kbd>C-q TAB</kbd> instead.
* <kbd>M-SPC</kbd> doesn't call `just-one-space`, and is used for auto-completion. Use <kbd>C-c \\</kbd> or <kbd>C-c C-\\</kbd> instead.
* When no region is selected, <kbd>C-w</kbd> will kill the current line and <kbd>M-w</kbd> will copy the current line.

## Scripts

All scripts are in the `script/` folder.

| Script             | Description                                                                                 |
| ------------------ | ------------------------------------------------------------------------------------------- |
| clean.sh           | Delete all compiled Emacs Lisp files.                                                       |
| fetch-info-docs.sh | Fetch info documentation. Available only on GNU/Linux.                                      |
