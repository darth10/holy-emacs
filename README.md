## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, C#, JavaScript, Ruby, Haskell and C.
* Uses God mode to reduce keystrokes. All normal key bindings work in God mode too; God mode is only used to minimize the number of keys pressed.
* Uses `j` as a sticky control key.
* Identical (almost) bindings across different major modes.
* Installs all required packages from GNU, Marmalade and MELPA repositories on startup.
* Includes helm, yasnippet and autocomplete.
* Git integration via magit.
* File and project search via ag.
* Manage sessions via workgroups.
* Quickly move regions using `M-<up>` and `M-<down>` like in Eclipse.
* Quickly create and navigate across bookmarks via breadcrumb.
* Automatically saves backup files to `~/.emacs-saves/`.
* Recomile entire `~/.emacs.d` using the `rebuild` function.
* Remember that the meta (`M-`) modifier is the same as prefixing the `C-[` key.

## Scripts

All scripts are in the `script/` folder.

| Script             | Description                                                                                 |
| ------------------ | ------------------------------------------------------------------------------------------- |
| clean.sh           | Delete all compiled Emacs Lisp files.                                                       |
| fetch-info-docs.sh | Fetch info documentation. Available only on GNU/Linux.                                      |
| build-ri-doc.sh    | Fetch and build Ruby ri documentation on Linux. Available only on GNU/Linux. Requires rvm.  |

## God mode

Use the escape key (`<escape>`) to toggle God mode. In God mode, `i` will also disable God mode.
Note that you can still use `C-[` instead of the escape key.
In God mode, you can simply use `[` for the escape key.

* `C-x C-c` becomes `xc`.
* `C-x (` becomes `x (`. Note the use of the space key.
* `M-x` becomes `gx`.
* `C-M-c` becomes `Gc`.
* `M-10 C-n` becomes `10n`.
* Use `.` to repeat the last God mode command.
* Use `q` followed by a single character to insert the character in God mode.

## Sticky control key

Press the `j` key twice in quick succession to emulate the control (`C-`) modifier.
If the `j` key is pressed while editing text, there will be a slight delay before it is shown in the buffer.
There are a few additional bindings that use a single quick `j` prefix.
All of these functions still have their original key bindings bound to them.
God mode does not affect the `j` sticky control key.
You can always toggle this feature using ``C-` ``.

* `jx` emulates the `C-x` prefix.
* `jc` emulates the `C-c` prefix.
* `jw` saves the buffer (`C-x C-s`).
* `jf` opens a file (`C-x C-f`).

## Global key bindings

| Keys                                            | God mode Keys   | Description                                                                                                                  |
| ----------------------------------------------- | --------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `C-x w`                                         | `x w`           | Save current buffer.                                                                                                         |
| `C-x "`                                         | `x "`           | Switch to `*scratch*` buffer in new frame.                                                                                   |
| `C-x C-'`                                       | `x'`            | Switch to `*scratch*` buffer in other window.                                                                                |
| `C-x '`                                         | `x '`           | Switch to `*scratch*` buffer.                                                                                                |
| `C-x M-[`                                       | `xg[`           | Switch to previous buffer.                                                                                                   |
| `C-x M-]`                                       | `xg]`           | Switch to next buffer.                                                                                                       |
| `C-x C-0`                                       | `x0`            | Delete window.                                                                                                               |
| `C-x C-1`                                       | `x1`            | Delete all windows except the current one.                                                                                   |
| `C-x C-2`                                       | `x2`            | Split window below.                                                                                                          |
| `C-x C-3`                                       | `x3`            | Split window right.                                                                                                          |
| `C-x C-9`                                       | `x9`            | Delete window and current frame.                                                                                             |
| `C-x C-5 C-0`                                   | `x50`           | Delete frame.                                                                                                                |
| `C-x C-5 C-1`                                   | `x51`           | Delete all frames except the current one.                                                                                    |
| `C-x C-5 C-2`                                   | `x52`           | Create new frame.                                                                                                            |
| `C-x <f3>` or `C-! C-p`                         | `!p`            | Show process list.                                                                                                           |
| `C-x <f5>` or `C-! C-k`                         | `!k`            | Compile using `compile` function.                                                                                            |
| `C-x <f7>` or `C-! C-e`                         | `!e`            | Open Eshell.                                                                                                                 |
| `C-x <f10>` or `C-' C-' C-=` or `C-' ' =`       | `''=`           | Start Ediff between buffers.     											                                                   |
| `C-x S-<f10>` or `C-' C-' C-+` or `C-' ' +`     | `''+`           | Start Ediff between files. 	     											                                               |
| `C-x <f11>` or `C-! C-c`                        | `!c`            | Show calendar. 	   	     	     											                                               |
| `C-x <C-M-RET>` or `C-x S-\`                    | `x S-\`         | Show init.el file.  	     	     											                                               |
| `C-x RET RET`                                   |                 | Set rectangular region mark.  	     	     											                                   |
| `C-x <f12>` or `C-! C-n`                        | `!n`            | Show calculator.                                                                                                             |
| `C-x ?`                                         | `x ?`           | Open man page. Available only on GNU/Linux.                                                                                  |
| `C-x C-y`                                       | `xy`            | Show kill ring.                                                                                                              |
| `C-x r =`                                       | `x r =`         | Reset highlighted diff (`diff-hl`) in current buffer.                                                                        |
| `C-<f2>` or `C-x :`                             | `x :`           | Helm i-menu. Useful for code navigation.                                                                                     |
| `M-]` or `C-[ ]`                                | `[ ]` or `g]`   | Highlight token under cursor. Use `[` or `]` to navigate to previous or next hit, or `ESC` to remove all highlighted tokens. |
| `C-[ C-[ ]`                                     |  `[[ ]`         | Remove all highlighted tokens.                                                                                               |
| `C-<f4>` or `C-' C-l` or `C-' l`                | `'l`            | Toggle highlight current line.                                                                                               |
| `C-<f6>` or `C-' C-n` or `C-' n`                | `'n`            | Toggle line numbers.                                                                                                         |
| `C-<f9>` or `C-' C-w` or `C-' w`                | `'w`            | Toggle truncate lines (word wrapping).                                                                                       |
| `C-c (`                                         | `c (`           | Toggle paredit-mode. Useful for editing Lisp code.                                                                           |
| `C-' C-' C-c` or `C-' ' c`                      | `''c`           | Toggle camel-case mode.                                                                                                      |
| `C-S-\`                                         | `S-\`           | Move to window.                                                                                                              |
| `C-+`                                           | `+`             | Interactively resize current window.                                                                                         |
| `C-?`                                           | `?`             | Look up any documentation. Changes behaviour depending on major mode, and defaults to available info documentation.          |
| `C-=`                                           | `=`             | Expand region.                                                                                                               |
| `C-~`                                           | `~`             | Show mark ring.                                                                                                              |
| ``C-` ``                                        | `` ` ``         | Toggle sticky control key (`j`).                                                                                             |
| `<f6>` or `C-%`                                 | `%`             | Move to matching parentheses.                                                                                                |
| `C-[ M-x` or `C-[ C-[ x`                        | `[[ x`          | Execute menu command.                                                                                                        |
| `M-<up>` or `M-p`                               | `gp`            | Move line or region up.                                                                                                      |
| `M-<down>` or `M-n`                             | `gn`            | Move line or region down.                                                                                                    |
| `M-<f5>` or `C-x C-a C-k` or `C-x a k`          | `xak`           | Recompile using `recompile` function.                                                                                        |
| `C-x C-j`                                       | `xj`            | Show directory.                                                                                                              |
| `C-c C-j`                                       | `cj`            | Show directory explorer.                                                                                                     |
| `C-c C-:`                                       | `c:`            | Show recently opened files.                                                                                                  |
| `C-c C-\` or `M-SPC`                            | `c\`            | Delete all spaces leaving one space.                                                                                         |
| `C-' C-' C-a` or `C-' ' a`                      | `''a`           | Toggle auto-completion.                                                                                                      |
| `C-' C-' C-q` or `C-' ' q`                      | `''q`           | Toggle auto fill mode.                                                                                                       |

### Git integration

| Keys                                  | God mode Keys    | Description                             |
| ------------------------------------- | ---------------- | --------------------------------------- |
| `C-: C-s` or `C-: C-:` or `C-: :`     | `:s` or `::`     | Show current git repository status.     |
| `C-: C-k`                             | `:k`             | Run `gitk`.                             |
| `C-: C-d`                             | `:d`             | `git diff` for current file.            |
| `C-: C-c C-d`                         | `:cd`            | `git diff` for current git repository.  |
| `C-: C-l`                             | `:l`             | Show current git repository log.        |
| `C-: C-f`                             | `:f`             | Find file in current git repository.    |
| `C-: <f10>` or `C-: C-=`              | `:=`             | Start `vc-ediff` for current file.      |
| `C-: <f3>` or `C-: M-s` or `M-s :`    | `:gs`            | `git grep` in current git repository.   |

### Search

Requires [ag](https://github.com/ggreer/the_silver_searcher).

| Keys                          | God mode Keys    | Description                     |
| ----------------------------- | ---------------- | ------------------------------- |
| `M-s a a` or `C-<f3>`         | `gs a a`         | Search in files.                |
| `M-s a r` or `C-S-<f3>`       | `gs a r`         | Search regexp in files.         |
| `M-s e a`                     | `gs e a`         | Search in project.              |
| `M-s e r`                     | `gs e r`         | Search regexp in project.       |
| `M-s o`                       | `gs o`           | Find occurences in buffer.      |
| `M-s i`                       | `gs i`           | Jump to occurences in buffer.   |
| `M-s G`                       | `gs G`           | Run grep.                       |
| `M-s g`                       | `gs g`           | Search using grep.              |

### Workgroups

Prefix is `C-x C-:`, or `x:` in God mode.

| Keys                          | God mode Keys    | Description                |
| ----------------------------- | ---------------- | -------------------------- |
| `C-x C-: C-v`                 | `x:v`            | Switch to workgroup.       |
| `C-x C-: C-a`                 | `x:a`            | Switch to next workgroup.  |
| `C-x C-: C-c`                 | `x:c`            | Create a new workgroup.    |
| `C-x C-: C-k`                 | `x:k`            | Kill workgroup.            |
| `C-x C-: ?`                   | `x: ?`           | Show help for workgroups.xs|

### Code folding

| Keys                      | God mode Keys     | Description                            |
| ------------------------- | ----------------- | -------------------------------------- |
| `C-c h`                   | `c h`             | Hide block.                            |
| `C-c g h`                 | `c g h`           | Hide all blocks.                       |
| `C-c s`                   | `c s`             | Show block.                            |
| `C-c g s`                 | `c g s`           | Show all blocks.                       |

### Multiple cursors

| Keys                      | God mode Keys     | Description                            |
| ------------------------- | ----------------- | -------------------------------------- |
| `C-x <C-RET>`             |                   | Edit lines with multiple cursors.      |
| `C->`                     | `>`               | Mark next line or word.                |
| `C-<`                     | `<`               | Mark preivous line or word.            |
| `C-c C-<f3>` or `C-c C->` | `c>`              | Mark all words like selected word.     |

### Bookmarks

| Keys                         | God mode Keys | Description                                  |
| ---------------------------- | ------------- | -------------------------------------------- |
| `C-c C-;`                    | `c;`          | Show bookmark list.                          |
| `C-c C-=`                    | `c=`          | Add bookmark.                                |
| `C-c C-#`                    | `c#`          | Delete all bookmarks.                        |
| `C-c C-<left>` or `C-c C-b`  | `cb`          | Move to previous bookmark.                   |
| `C-c C-<right>` or `C-c C-f` | `cf`          | Move to next bookmark.                       |
| `C-c C-<up>` or `C-c C-p`    | `cp`          | Move to previous bookmark in current buffer. |
| `C-c C-<down>` or `C-c C-n`  | `cn`          | Move to next bookmark in current buffer.     |

### org-mode

| Keys            | God mode Keys | Description              |
| --------------- | ------------- | ------------------------ |
| `C-c b`         | `c b`         | List org-mode buffers.   |

## Mode-specific key bindings

### Ediff

| Keys               | Description                          |
| ------------------ | ------------------------------------ |
| `M-<down>` or `n`  | Next diff.                           |
| `M-<up>` or `p`    | Previous diff.                       |
| `M-<right>` or `a` | Move diff from left to right buffer. |
| `M-<left>` or `b`  | Move diff from right to left buffer. |

### dired-mode

| Keys            | Description                            |
| --------------- | -------------------------------------- |
| `C-x C-/`       | Switch to editable (wdired-mode).      |

### org-mode

| Keys            | God mode Keys | Description                                     |
| --------------- | ------------- | ----------------------------------------------- |
| `C-x t`         | `x t`         | Show org-mode timeline.                         |
| `C-c a`         | `c a`         | Show org-mode agenda.                           |
| `C-c c`         | `c c`         | Capture org-mode template.                      |
| `C-c l`         | `c l`         | Store org-mode link.                            |

### Paredit mode

| Keys                                                      | God mode Keys     | Description                               |
| --------------------------------------------------------- | ----------------- | ----------------------------------------- |
| `C-[ <right>` or `C-, C-f` or `C-, f`                     | `,f`              | Slurp into current sexp from right.	    |
| `C-[ <left>` or `C-, C-b` or `C-, b`                      | `,b`              | Barf from current sexp from right.        |
| `C-[ C-[ <right>` or `C-, C-, C-f` or `C-, , f`           | `,,f`             | Barf from current sexp from left.	        |
| `C-[ C-[ <left>` or `C-, C-, C-b` or `C-, , b`            | `,,b`             | Slurp into current sexp from left.	    |
| `C-[ C-[ <down>` or `C-, C-k` or `C-, k`                  | `,k`              | Splice current sexp by killing forward.   |
| `C-[ C-[ <up>` or `C-, C-, C-k` or `C-, , k`              | `,,k`             | Splice current sexp by killing backward.  |

### All Lisp modes

Available in Clojure, Scheme and Emacs Lisp modes.

| Keys                   | God mode Keys | Description                              |
| ---------------------- | ------------- | ---------------------------------------- |
| `C-x C-e`              | `xe`          | Evaluate last sexp.                      |
| `C-c C-k`              | `ck`          | Evaluate current buffer.                 |
| `C-c C-l`              | `cl`          | Load file.                               |
| `C-<f12>` or `C-' C-s` | `'s`          | Toggle highlight s-expresion.            |

### GUD mode

Requires GUD debugger to be active.
Supports `gdb` for C, `pdb` for Python, and `ruby -r debug` for Ruby.

| Keys                                        | God mode Keys        | Description                              |
| ------------------------------------------- | -------------------- | ---------------------------------------- |
| `<f8>` or `C-x C-a C-x C-r` or `C-x a x r`  | `xaxr`               | Debug program/script.                    |
| `<f5>` or `C-x C-a C-r` or `C-x a r`        | `xar`                | Resume or continue execution.            |
| `<f9>` or `C-x C-a C-b` or `C-x a b`        | `xab`                | Set breakpoint.                          |
| `S-<f9>`or `C-x C-a C-d` or `C-x a d`       | `xad`                | Delete breakpoint.                       |
| `<f10>` or `C-x C-a C-n` or `C-x a n`       | `xan`                | Next statement.                          |
| `<f11>` or `C-x C-a C-s` or `C-x a s`       | `xas`                | Step into statement.                     |
| `S-<f11>` or `C-x C-a C-f` or `C-x a f`     | `xaf`                | Finish execution of current function.    |

### Clojure

Requires [Leiningen](https://github.com/technomancy/leiningen).

| Keys                                 | God mode Keys        | Description                                    |
| ------------------------------------ | -------------------- | ---------------------------------------------- |
| `C-<f5>`, `C-x C-a C-a` or `C-x a a` | `xaa`                | Load and switch to current namespace in nREPL. |
| `C-<f8>` or `C-! C-o`                | `!o`                 | Connect to nREPL server.                       |
| `C-<f10>` or `C-! C-r`               | `!r`                 | Start nREPL server and connect.                |
| `C-?`                                | `?`                  | Show nREPL documentation for current word.     |
| `C-x T`                              | `x T`                | Run all tests for current namespace.           |
| `C-x t`                              | `x t`                | Run current test.                              |

### Emacs Lisp

| Keys                                 | God mode Keys        | Description                                                                                          |
| ------------------------------------ | -------------------- | ---------------------------------------------------------------------------------------------------- |
| `C-<f5>`, `C-x C-a C-a` or `C-x a a` | `xaa`                | Evaluate current buffer. When used in the `*scratch*` buffer, it evaluates and prints the last sexp. |
| `C-<f10>` or `C-! C-r`               | `!r`                 | Open Eshell.                                                                                         |


### Scheme

Requires Racket and/or Guile Scheme.

| Keys                                   | God mode Keys        | Description                           |
| -------------------------------------- | -------------------- | ------------------------------------- |
| `C-<f5>`, `C-x C-a C-a` or `C-x a a`   | `xaa`                | Evaluate current buffer in REPL.      |
| `C-<f10>` or `C-! C-r`                 | `!r`                 | Start Scheme REPL server and connect. |


<h3> C# </h3>

Requires [Omnisharp](https://github.com/nosami/Omnisharp).

| Keys            | Description                                           |
| --------------- | ----------------------------------------------------- |
| `C-<f10>`       | Start Omnisharp server.                               |
| `C-x <f5>`      | Build current solution.                               |
| `M-SPC`         | Autocomplete symbol. A `.` invokes this function too. |
| `C-?`           | Show overloads of symbol at point.                    |
| `<f12>`         | Go to definition.                                     |
| `C-c F`         | Format document.                                      |
| `S-<f12>`       | Find all references.                                  |


### JavaScript

Requires Node.js, [swank-js](https://github.com/swank-js/swank-js) and [tern](https://github.com/marijnh/tern).

| Keys                                 | God mode Keys | Description                                 |
| ------------------------------------ | ------------- | ------------------------------------------- |
| `M-SPC`                              |               | Autocomplete symbol.                        |
| `C-x C-e`                            | `xe`          | Evaluate current statement.                 |
| `C-<f5>`, `C-x C-a C-a` or `C-x a a` | `xaa`         | Evaluate current buffer and switch to REPL. |
| `C-<f8>` or `C-! C-o`                | `!o`          | Connect to SwankJS sever.                   |
| `C-<f10>` or `C-! C-r`               | `!r`          | Start SwankJS REPL.                         |

### Haskell

Requires Haskell platform and `haskell-doc`.

| Keys                                  | God mode Keys | Description                                 |
| ------------------------------------- | ------------- | ------------------------------------------- |
| `C-<f5>`, `C-x C-a C-a` or `C-x a a`  | `xaa`         | Evaluate current buffer and switch to REPL. |
| `C-<f10>` or `C-! C-r`                | `!r`          | Start GHCi REPL.                            |

### Ruby

Requires Ruby and `rake`.

| Keys            | Description                             |
| --------------- | --------------------------------------- |
| `C-<f5>`        | Evaluate current buffer in REPL.        |
| `C-<f8>`        | Run Rake task.                          |
| `C-<f10>`       | Start irb REPL.                         |
| `C-<f11>`       | Start Ruby debugger.                    |
| `C-?`           | Show ri documentation for current word. |
| `C-x T`         | Run all tests via `rake test`.          |

### C

Available only on GNU/Linux.

| Keys            | Description                   |
| --------------- | ----------------------------- |
| `C-<f11>`       | Start gdb.                    |
| `C-<f10>`       | Switch to gdb console buffer. |
| `C-<f12>`       | Display disassembly.          |

### HTML

| Keys            | Description                   |
| --------------- | ----------------------------- |
| `M-SPC`         | Zencoding expand line.        |
