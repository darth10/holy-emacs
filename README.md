## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, C#, JavaScript, Ruby, Haskell and C.
* Emphasis on use of function keys.
* Uses God Mode to reduce keystrokes. All normal key bindings work in God Mode too; God Mode is only used to minimize the number of keys pressed.
* Identical (almost) bindings across different major modes.
* Installs all packages from Marmalade and MELPA repositories on startup.
* Includes helm and autocomplete.
* Includes code snippets via yasnippet. Apart from those in yasnippet-bundle, there are several custom snippets for Clojure, JavaScript, Haskell and Scheme.
* Git integration via magit.
* File and project search via ag.
* Quickly move regions using `M-<up>` and `M-<down>` like in Eclipse.
* Quickly create and navigate across bookmarks via breadcrumb.
* Automatically saves backup files to `~/.emacs-saves/`.
* Recomile entire `~/.emacs.d` using the `rebuild` function.
* Remember that the `M` modifier is the same as prefixing the `ESC` or `C-[` key(s).

## Scripts

All scripts are in the `script/` folder.

| Script             | Description                                                                                 |
| ------------------ | ------------------------------------------------------------------------------------------- |
| clean.sh           | Delete all compiled Emacs Lisp files.                                                       |
| fetch-info-docs.sh | Fetch info documentation. Available only on GNU/Linux.                                      |
| build-ri-doc.sh    | Fetch and build Ruby ri documentation on Linux. Available only on GNU/Linux. Requires rvm.  |

## God Mode

Use `M-SPC` or `ESC SPC` to toggle God Mode. In God Mode, `i` will also disable God Mode.

* `C-x C-c` becomes `xc`.
* `C-x (` becomes `x (`. Note the use of the space key.
* `M-x` becomes `gx`.
* `C-M-c` becomes `Gc`.
* `M-10 C-n` becomes `10n`.
* Use `z` to repeat the last God Mode command.

## Global key kindings

| Keys                         | God Mode Keys   | Description                                                                                                                  |
| ---------------------------- | --------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `C-x <f3>`                   |                 | Show process list.                                                                                                           |
| `C-x <f5>`                   |                 | Compile using `compile` function.                                                                                            |
| `C-x <f7>` or `C-c E`        | `c E`           | Open Eshell.                                                                                                                 |
| `C-x <f10>` or `C-c C-c =`   | `cc =`          | Start Ediff between buffers.     											                                                |
| `C-x S-<f10>` or `C-c C-c +` | `cc +`          | Start Ediff between files. 	     											                                                |
| `C-x <f11>`                  |                 | Show calendar. 	   	     	     											                                                |
| `C-x <C-M-RET>`              |                 | Show init.el file.  	     	     											                                                |
| `C-x RET RET`                |                 | Set rectangular region mark.  	     	     											                                    |
| `C-x <f12>`                  |                 | Show calculator.                                                                                                             |
| `C-x ?`                      | `x ?`           | Open man page. Available only on GNU/Linux.                                                                                  |
| `C-x C-y`                    | `xy`            | Show kill ring.                                                                                                              |
| `C-x G`                      | `x G`           | Run grep.                                                                                                                    |
| `C-x g`                      | `x g`           | Run recursive grep. Useful for searching in files.                                                                           |
| `C-<f2>`                     |                 | Helm i-menu. Useful for code navigation.                                                                                     |
| `M-]` or `C-[ ]`             | `g]`            | Highlight token under cursor. Use `[` or `]` to navigate to previous or next hit, or `ESC` to remove all highlighted tokens. |
| `ESC M-]` or `C-[ C-[ ]`     |                 | Remove all highlighted tokens.                                                                                               |
| `C-<f4>`                     |                 | Toggle highlight current line.                                                                                               |
| `C-<f6>`                     |                 | Toggle line numbers.                                                                                                         |
| `C-<f9>`                     |                 | Toggle truncate lines (word wrapping).                                                                                       |
| `C-c (`                      | `c (`           | Toggle paredit-mode. Useful for editing Lisp code.                                                                           |
| `C-' C-C`                    | `'C`            | Toggle paredit-mode. Useful for editing Lisp code.                                                                           |
| `C-S-\`                      | `S-\`           | Move to window.                                                                                                              |
| `C-+`                        | `+`             | Interactively resize current window.                                                                                         |
| `C-?`                        | `?`             | Look up any documentation. Changes behaviour depending on major mode, and defaults to available info documentation.          |
| `C-=`                        | `=`             | Expand region.                                                                                                               |
| `<f11>` or `C-%`             | `%`             | Move to matching parentheses.                                                                                                |
| `C-<XF86Back>`               | `<XF86Back>`    | Previous buffer. Available only on ThinkPad keyboards.                                                                       |
| `C-<XF86Forward>`            | `<XF86Forward>` | Next buffer. Available only on ThinkPad keyboards.                                                                           |
| `ESC M-x` or `ESC ESC x`     |                 | Execute menu command.                                                                                                        |
| `M-<up>` or `M-p`            | `gp`            | Move line or region up.                                                                                                      |
| `M-<down>` or `M-n`          | `gn`            | Move line or region down.                                                                                                    |
| `M-<f5>`                     |                 | Recompile using `recompile` function.                                                                                        |
| `C-x C-j`                    | `xj`            | Show directory.                                                                                                              |
| `C-c C-j`                    | `cj`            | Show directory explorer.                                                                                                     |
| `C-c C-c C-a`                | `cca`           | Toggle auto-completion.                                                                                                      |

### Git integration

| Keys                         | God Mode Keys    | Description                             |
| ---------------------------- | ---------------- | --------------------------------------- |
| ``C-` C-k``                  | `` `k``          | Run `gitk`.                             |
| ``C-` C-d``                  | `` `d``          | `git diff` for current file.            |
| ``C-` C-c C-d``              | `` `cd``         | `git diff` for current git repository.  |
| ``C-` C-s``                  | `` `s``          | Show current git repository status.     |
| ``C-` C-l``                  | `` `l``          | Show current git repository log.        |
| ``C-` C-f``                  | `` `f``          | Find file in current git repository.    |
| ``C-` <f3>`` or ``C-` C-/``  | `` `/``          | `git grep` in current git repository.   |
| ``C-` <f10>`` or ``C-` C-=`` | `` `=``          | Start `vc-ediff` for current file.  `   |

### Search

Requires ag.

| Keys                          | God Mode Keys    | Description                |
| ----------------------------- | ---------------- | -------------------------- |
| `C-<f3>` or `C-c C-/`         | `c/`             | Search in files.           |
| `C-S-<f3>` or `C-c C-?`       | `c?`             | Search regexp in files.    |
| `C-c <f3>` or `C-c C-c C-/`   | `cc/`            | Search in project.         |
| `C-c S-<f3>` or `C-c C-c C-/` | `cc?`            | Search regexp in project.  |


### Code folding TODO

| Keys                      | God Mode Keys     | Description                            |
| ------------------------- | ----------------- | -------------------------------------- |
| `C-c C-h`                 | `ch`              | Hide block.                            |
| `C-c C-g C-h`             | `cgh`             | Hide all blocks.                       |
| `C-c C-s`                 | `cs`              | Show block.                            |
| `C-c C-g C-s`             | `cgs`             | Show all blocks.                       |

### Multiple cursors

| Keys                      | God Mode Keys     | Description                            |
| ------------------------- | ----------------- | -------------------------------------- |
| `C-x <C-RET>`             |                   | Edit lines with multiple cursors.      |
| `C->`                     | `>`               | Mark next line or word.                |
| `C-<`                     | `<`               | Mark preivous line or word.            |
| `C-c C-<f3>` or `C-c C->` | `c>`              | Mark all words like selected word.     |

### Bookmarks

| Keys                         | God Mode Keys | Description                                  |
| ---------------------------- | ------------- | -------------------------------------------- |
| `C-c C-;`                    | `c;`          | Show bookmark list.                          |
| `C-c C-=`                    | `c=`          | Add bookmark.                                |
| `C-c C-#`                    | `c#`          | Delete all bookmarks.                        |
| `C-c C-<left>` or `C-c C-b`  | `cb`          | Move to previous bookmark.                   |
| `C-c C-<right>` or `C-c C-f` | `cf`          | Move to next bookmark.                       |
| `C-c C-<up>` or `C-c C-p`    | `cp`          | Move to previous bookmark in current buffer. |
| `C-c C-<down>` or `C-c C-n`  | `cn`          | Move to next bookmark in current buffer.     |

### dired-mode

| Keys            | Description                            |
| --------------- | -------------------------------------- |
| `C-x C-/`       | Switch to editable (wdired-mode).      |

### org-mode

| Keys            | God Mode Keys | Description                                     |
| --------------- | ------------- | ----------------------------------------------- |
| `C-x t`         | `x t`         | Show org-mode timeline.                         |
| `C-c a`         | `c a`         | Show org-mode agenda.                           |
| `C-c b`         | `c b`         | List org-mode buffers. Can be used in any mode. |
| `C-c c`         | `c c`         | Capture org-mode template.                      |
| `C-c l`         | `c l`         | Store org-mode link.                            |

## Mode-specific key bindings

### Paredit mode

| Keys                      | God Mode Keys | Description                              |
| ------------------------- | ------------- | ---------------------------------------- |
| `M-<left>` or `M-g b`     | `ggb`         | Barf from current sexp from right.       |
| `M-<right>` or `M-g f`    | `ggf`         | Slurp into current sexp from right.	   |
| `ESC M-<left>` or `M-g B` | `ggB`         | Slurp into current sexp from left.	   |
| `ESC M-<right>` or `M-g F`| `ggF`         | Barf from current sexp from left.	       |
| `ESC M-<up>` or `M-g K`   | `ggK`         | Splice current sexp by killing backward. |
| `ESC M-<down>` or `M-g k` | `ggk`         | Splice current sexp by killing forward.  |

### All Lisp modes

Available in Clojure, Scheme and Emacs Lisp modes.

| Keys            | Description                              |
| --------------- | ---------------------------------------- |
| `C-<f12>`       | Toggle highlight s-expresion.            |

### Clojure

Requires Leiningen with nREPL middleware.

| Keys            | Description                                    |
| --------------- | ---------------------------------------------- |
| `C-<f5>`        | Load and switch to current namespace in nREPL. |
| `C-<f8>`        | Connect to nREPL server.                       |
| `C-<f10>`       | Start nREPL server and connect.                |
| `C-?`           | Show nREPL documentation for current word.     |
| `C-x T`         | Run all tests for current namespace.           |
| `C-x t`         | Run current test.                              |

### Emacs Lisp

| Keys         | Description                                                                           |
| --------------- | ------------------------------------------------------------------------------------- |
| `C-<f5>`        | Evaluate current buffer. When used in the scratch buffer, it evaluates the last sexp. |
| `C-<f10>`       | Start iELM REPL.                                                                      |


### Scheme

Requires Racket or Guile Scheme.

| Keys            | Description                          |
| --------------- | ------------------------------------ |
| `C-<f5>`        | Evaluate current buffer in REPL.     |
| `C-<f10>`       | Start Scheme REPL server and connect.|


<h3> C# </h3>

Requires Omnisharp.

| Keys            | Description                                           |
| --------------- | ----------------------------------------------------- |
| `C-<f10>`       | Start Omnisharp server.                               |
| `C-x <f5>`      | Build current solution.                               |
| `C-x SPC`       | Autocomplete symbol. A `.` invokes this function too. |
| `C-?`           | Show overloads of symbol at point.                    |
| `<f12>`         | Go to definition.                                     |
| `M-p`           | Format document.                                      |
| `S-<f12>`       | Find all references.                                  |



### JavaScript

Requires Node.js.

| Keys            | Description                                 |
| --------------- | ------------------------------------------- |
| `C-<f5>`        | Evaluate current buffer and switch to REPL. |
| `C-<f10>`       | Start Node.js REPL.                         |

### Haskell

Requires Haskell platform.

| Keys            | Description                                 |
| --------------- | ------------------------------------------- |
| `C-<f5>`        | Evaluate current buffer and switch to REPL. |
| `C-<f10>`       | Start GHCi REPL.                            |
| `C-TAB`         | Complete current word.                      |

### Ruby

Requires Ruby and Rake.

| Keys            | Description                             |
| --------------- | --------------------------------------- |
| `C-<f5>`        | Evaluate current buffer in REPL.        |
| `C-<f8>`        | Run Rake task.                          |
| `C-<f10>`       | Start irb REPL.                         |
| `C-?`           | Show ri documentation for current word. |
| `C-x T`         | Run all tests via `rake test`.          |

### C

Available only on GNU/Linux.

| Keys            | Description                   |
| --------------- | ----------------------------- |
| `C-<f5>`        | Run in gdb.                   |
| `C-<f10>`       | Start gdb.                    |
| `C-<f11>`       | Switch to gdb console buffer. |
| `C-<f12>`       | Display disassembly.          |
| `<f5>`          | Step into in debug.           |
| `<f6>`          | Next in debug.                |
| `<f7>`          | Step out in debug.            |
| `<f8>`          | Continue in debug.            |
