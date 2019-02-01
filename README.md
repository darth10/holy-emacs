```
     __          __
    / /_  ____  / /_  __    ___  ____ ___  ____ ___________
   / __ \/ __ \/ / / / /   / _ \/ __ ‘__ \/ __ ‘/ ___/ ___/
  / / / / /_/ / / /_/ /   /  __/ / / / / / /_/ / /__(__  )
 /_/ /_/\____/_/\__, /    \___/_/ /_/ /_/\__,_/\___/____/
               /____/
```
## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, C#, JavaScript, Haskell, Python, Ruby, PHP and C.
* Installs all required packages from GNU, Marmalade and MELPA repositories on startup.
* Includes `magit`, `helm`, `projectile`, `company` and `yasnippet`.
* Uses `god-mode` for modal editing. All key bindings work in `god-mode` and it's only used to reduce the usage of modifier keys.
* Uses `use-package` and `bind-key`. <kbd>C-h C-l</kbd> (or <kbd>M-x describe-personal-keybindings</kbd>) will display all available key bindings.
* Uses `which-key` to interactively describe key bindings.
* Uses `desktop-save-mode` to manage sessions and buffers.
* Uses `editorconfig` to handle multiple indentation styles.
* Automatically saves backup files to `~/.emacs.bak/`.
* Upgrade all packages using <kbd>M-x core/upgrade-packages</kbd> or `make upgrade`.
* Recompile all packages and configuration using <kbd>M-x core/byte-recompile-files</kbd> or `make recompile`.

## Installation

```sh
git clone git@github.com:darth10/holy-emacs.git ~/.emacs.d
cd ~/.emacs.d
# edit init.el if needed
make
```

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

If you prefer to not use `god-mode`, toggle the `core-enable-god-mode` variable through 
`M-x customize-group RET holy-emacs RET`.

## Overridden default key bindings

Some of the default Emacs key bindings are changed, as follows.

* <kbd>C-s</kbd> will save the current buffer (`save-buffer`).
* <kbd>M-s s</kbd> and <kbd>M-s r</kbd> will perform an incremental
  forward and backward search respectively (`isearch-forward` and `isearch-backward`).
* <kbd>M-i</kbd> won't add indentation to current line (`tab-to-tab-stop`).
  Use <kbd>M-[</kbd> or <kbd>C-q TAB</kbd> instead.
* <kbd>M-SPC</kbd> won't delete all space leaving one space (`just-one-space`).
  Use <kbd>C-c \\</kbd> or <kbd>C-c C-\\</kbd> instead.
* <kbd>C-x a n</kbd> won't move the cursor to the next expansion slot (`expand-jump-to-next-slot`).
* <kbd>F10</kbd> won't open the main menu (`menu-bar-open`).
* <kbd>C-z</kbd> won't minimize Emacs (`suspend-frame`).
* <kbd>C-w</kbd> will kill the current line and <kbd>M-w</kbd> will
  copy the current line when no region is selected.
* <kbd>C-x C-c</kbd> will exit Emacs with a confirmation.
