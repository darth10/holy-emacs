## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, JavaScript, Ruby, Haskell and C.
* Emphasis on use of function keys.
* Identical (almost) bindings across different major modes.
* Includes helm and autocomplete.
* Includes code snippets via yasnippet. Apart from those in yasnippet-bundle, there are several custom snippets for Clojure, JavaScript, Haskell and Scheme.
* Git integration via magit.
* Quickly move regions using `M-<up>` and `M-<down>` like in Eclipse.
* Quickly create and navigate across bookmarks via breadcrumb.
* Automatically save backup files to `~/.emacs-saves/`.
* Recomile entire `~/.emacs.d` using the `recompile-emacs-d` function.
* Info documentation. Use `fetch-info-docs.sh`.
* Ruby ri documentation on Linux. Use `build-ri-doc.sh`.

## Global key kindings

| Binding           | Description                                                                                                                  |
| ----------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `C-x <f3>`        | Show process list.                                                                                                           |
| `C-x <f7>`        | Open terminal. Available only on GNU/Linux.                                                                                  |
| `C-x <f10>`       | Start Ediff between buffers.     											                                                   |
| `C-x S-<f10>`     | Start Ediff between files. 	     											                                               |
| `C-x <f11>`       | Show calendar. 	   	     	     											                                               |
| `C-x <C-M-return>`| Show init.el file.  	     	     											                                               |
| `C-x <f12>`       | Show calculator.                                                                                                             |
| `C-x ?`           | Open man page. Available only on GNU/Linux.                                                                                  |
| `C-x G`           | Run grep.                                                                                                                    |
| `C-x g`           | Run recursive grep. Helpful for searching in files.                                                                          |
| `C-<f2>`          | Helm i-menu. Useful for code navigation.                                                                                     |
| `C-<f3>`          | Highlight token under cursor. Use `[` or `]` to navigate to previous or next hit, or `ESC` to remove all highlighted tokens. |
| `C-S-<f3>`        | Remove all highlighted tokens.                                                                                               |
| `C-<f4>`          | Toggle highlight current line.                                                                                               |
| `C-<f6>`          | Toggle line numbers.                                                                                                         |
| `C-<f9>`          | Toggle truncate lines (word wrapping).                                                                                       |
| `C-c (`           | Toggle paredit-mode. Useful for editing Lisp code.                                                                           |
| `C-S-\`	        | Move to window.                                                                                                              |
| `C-+`             | Interactively resize current window.                                                                                         |
| `C-?`             | Look up any documentation. Changes behaviour depending on major mode, and defaults to available info documentation.          |
| `C-<XF86Back>`    | Previous buffer (Thinkpad only).                                                                                             |
| `C-<XF86Forward>` | Next buffer (Thinkpad only).                                                                                                 |
| `M-<up>`          | Move line or region up.                                                                                                      |
| `M-<down>`        | Move line or region down.                                                                                                    |

### Git integration

| Binding         | Description                            |
| --------------- | -------------------------------------- |
| `M-g <f10>`     | Start `vc-ediff` for current file.     |
| `M-g d`         | `git diff` for current file.           |
| `M-g M-d`       | `git diff` for current git repository. |
| `M-g M-s`       | Show current git repository status.    |
| `M-g M-l`       | Show current git repository log.       |
| `M-g M-f`       | Find file in current git repository.   |
| `M-g M-r`       | `git grep` in current git repository.  |

### Multiple cursors

| Binding         | Description                            |
| --------------- | -------------------------------------- |
| `C-x <C-return>`| Edit lines with multiple cursors.      |
| `C->`           | Mark next line or word.                |
| `C-<`           | Mark preivous line or word.            |
| `C-c C-<f3>`    | Mark all words like selected word.     |

### Bookmarks

| Binding         | Description                                  |
| --------------- | -------------------------------------------- |
| `C-c M-b`       | Show bookmark list.                          |
| `C-c C-b`       | Add bookmark.                                |
| `C-c C-S-b`     | Delete all bookmarks.                        |
| `C-c C-<left>`  | Move to previous bookmark.                   |
| `C-c C-<right>` | Move to next bookmark.                       |
| `C-c C-<up>`    | Move to previous bookmark in current buffer. |
| `C-c C-<down>`  | Move to next bookmark in current buffer.     |

### org-mode

| Binding         | Description                |
| --------------- | -------------------------- |
| `C-c a`         | Show org-mode agenda.      |
| `C-c a`         | Show org-mode timeline.    |
| `C-c b`         | List org-mode buffers.     |
| `C-c c`         | Capture org-mode template. |
| `C-c l`         | Store org-mode link.       |

## Mode-specific key bindings

### Paredit mode

| Binding         | Description                              |
| --------------- | ---------------------------------------- |
| `M-<left>`      | Barf from current sexp from right.       |
| `M-<right>`     | Slurp into current sexp from right.	     |
| `ESC M-<left>`  | Slurp into current sexp from left.	     |
| `ESC M-<right>` | Barf from current sexp from left.	     |
| `ESC M-<up>`    | Splice current sexp by killing backward. |
| `ESC M-<down>`  | Splice current sexp by killing forward.  |

### Clojure

Requires Leiningen with nREPL middleware.

| Binding         | Description                                    |
| --------------- | ---------------------------------------------- |
| `C-<f5>`        | Load and switch to current namespace in nREPL. |
| `C-<f8>`        | Connect to nREPL server.                       |
| `C-<f10>`       | Start nREPL server and connect.                |
| `C-?`           | Show nREPL documentation for current word.     |
| `C-x T`         | Run all tests for current namespace.           |
| `C-x t`         | Run current test.                              |

### Emacs Lisp

| Binding         | Description                                                                           |
| --------------- | ------------------------------------------------------------------------------------- |
| `C-<f5>`        | Evaluate current buffer. When used in the scratch buffer, it evaluates the last sexp. |
| `C-<f10>`       | Start iELM REPL.                                                                      |

### JavaScript

Requires Node.js.

| Binding         | Description                                 |
| --------------- | ------------------------------------------- |
| `C-<f5>`        | Evaluate current buffer and switch to REPL. |
| `C-<f10>`       | Start Node.js REPL.                         |

### Haskell

Requires Haskell platform.

| Binding         | Description                                 |
| --------------- | ------------------------------------------- |
| `C-<f5>`        | Evaluate current buffer and switch to REPL. |
| `C-<f10>`       | Start GHCi REPL.                            |
| `C-TAB`         | Complete current word.                      |

### Ruby

Requires Ruby and Rake.

| Binding         | Description                             |
| --------------- | --------------------------------------- |
| `C-<f5>`        | Evaluate current buffer in REPL.        |
| `C-<f8>`        | Run Rake task.                          |
| `C-<f10>`       | Start irb REPL.                         |
| `C-?`           | Show ri documentation for current word. |
| `C-x T`         | Run all tests via `rake test`.          |

### Guile Scheme

Available only on GNU/Linux. Requires Guile and libguile.

| Binding         | Description                          |
| --------------- | ------------------------------------ |
| `C-<f5>`        | Evaluate current buffer in REPL.     |
| `C-<f8>`        | Connect to Guile REPL server.        |
| `C-<f10>`       | Start Guile REPL server and connect. |

### C

Available only on GNU/Linux.

| Binding         | Description                   |
| --------------- | ----------------------------- |
| `C-<f5>`        | Run in gdb.                   |
| `C-<f10>`       | Start gdb.                    |
| `C-<f11>`       | Switch to gdb console buffer. |
| `C-<f12>`       | Display disassembly.          |
| `<f5>`          | Step into in debug.           |
| `<f6>`          | Next in debug.                |
| `<f7>`          | Step out in debug.            |
| `<f8>`          | Continue in debug.            |
