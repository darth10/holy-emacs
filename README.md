## Overview

* Support for editing/debugging Clojure, Emacs Lisp, Scheme, C#, JavaScript, PHP, Ruby, Haskell and C.
* Uses God mode to reduce keystrokes. All normal key bindings work in God mode too; God mode is only used to minimize the number of keys pressed.
* Uses <kbd>j</kbd> as a sticky control key.
* Identical (almost) bindings across different major modes.
* Installs all required packages from GNU, Marmalade and MELPA repositories on startup.
* Includes helm, yasnippet and autocomplete.
* Git integration via magit.
* File and project search via ag.
* Manage sessions via workgroups.
* Quickly move regions using <kbd>M-<up></kbd> and <kbd>M-<down></kbd> like in Eclipse.
* Quickly create and navigate across bookmarks via breadcrumb.
* Automatically saves backup files to `~/.emacs-saves/`.
* Recomile entire `~/.emacs.d` using the `rebuild` function.
* Remember that the meta (<kbd>M-</kbd>) modifier is the same as prefixing the <kbd>C-[</kbd> key.

## Scripts

All scripts are in the `script/` folder.

| Script             | Description                                                                                 |
| ------------------ | ------------------------------------------------------------------------------------------- |
| clean.sh           | Delete all compiled Emacs Lisp files.                                                       |
| fetch-info-docs.sh | Fetch info documentation. Available only on GNU/Linux.                                      |
| build-ri-doc.sh    | Fetch and build Ruby ri documentation on Linux. Available only on GNU/Linux. Requires rvm.  |

## God mode

Use the escape key (<kbd><escape></kbd>) to toggle God mode. In God mode, <kbd>i</kbd> will also disable God mode.
Note that you can still use <kbd>C-[</kbd> instead of the escape key.
In God mode, you can simply use <kbd>[</kbd> for the escape key.

* <kbd>C-x C-c</kbd> becomes <kbd>xc</kbd>.
* <kbd>C-x (</kbd> becomes <kbd>x (</kbd>. Note the use of the space key.
* <kbd>M-x</kbd> becomes <kbd>gx</kbd>.
* <kbd>C-M-c</kbd> becomes <kbd>Gc</kbd>.
* <kbd>M-10 C-n</kbd> becomes <kbd>10n</kbd>.
* Use <kbd>.</kbd> to repeat the last God mode command.
* Use <kbd>q</kbd> followed by a single character to insert the character in God mode.

## Sticky control key

Press the <kbd>j</kbd> key twice in quick succession to emulate the control (<kbd>C-</kbd>) modifier.
If the <kbd>j</kbd> key is pressed while editing text, there will be a slight delay before it is shown in the buffer.
There are a few additional bindings that use a single quick <kbd>j</kbd> prefix.
All of these functions still have their original key bindings bound to them.
God mode does not affect the <kbd>j</kbd> sticky control key.
You can always toggle this feature using <kbd>C-`</kbd>.

* <kbd>jx</kbd> emulates the <kbd>C-x</kbd> prefix.
* <kbd>jc</kbd> emulates the <kbd>C-c</kbd> prefix.
* <kbd>jw</kbd> saves the buffer (<kbd>C-x C-s</kbd>).
* <kbd>jf</kbd> opens a file (<kbd>C-x C-f</kbd>).

## Overridden default key bindings

Some of the default Emacs key bindings are changed, as follows.

* <kbd>C-x C-c</kbd> will exit Emacs with a confirmation.
* <kbd>C-x a n</kbd> and <kbd><f10></kbd> don't have their usual behaviour, as they are used by GUD key bindings.
* <kbd>M-SPC</kbd> doesn't call `just-one-space`, and is used for auto-completion. Use <kbd>C-c \\</kbd> or <kbd>C-c C-\\</kbd> instead.
* <kbd>C-z</kbd> doesn't minimize Emacs.


## Global key bindings

| Keys                                                            | God mode Keys                   | Description                                                                                                                  |
| --------------------------------------------------------------- | ------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| <kbd>C-x w</kbd>                                                | <kbd>x w</kbd>                  | Save current buffer.                                                                                                         |
| <kbd>C-x "</kbd>                                                | <kbd>x "</kbd>                  | Switch to `*scratch*` buffer in new frame.                                                                                   |
| <kbd>C-x C-'</kbd>                                              | <kbd>x'</kbd>                   | Switch to `*scratch*` buffer in other window.                                                                                |
| <kbd>C-x '</kbd>                                                | <kbd>x '</kbd>                  | Switch to `*scratch*` buffer.                                                                                                |
| <kbd>C-x M-[</kbd>                                              | <kbd>xg[</kbd>                  | Switch to previous buffer.                                                                                                   |
| <kbd>C-x M-]</kbd>                                              | <kbd>xg]</kbd>                  | Switch to next buffer.                                                                                                       |
| <kbd>C-x C-0</kbd>                                              | <kbd>x0</kbd>                   | Delete window.                                                                                                               |
| <kbd>C-x C-1</kbd>                                              | <kbd>x1</kbd>                   | Delete all windows except the current one.                                                                                   |
| <kbd>C-x C-2</kbd>                                              | <kbd>x2</kbd>                   | Split window below.                                                                                                          |
| <kbd>C-x C-3</kbd>                                              | <kbd>x3</kbd>                   | Split window right.                                                                                                          |
| <kbd>C-x C-9</kbd>                                              | <kbd>x9</kbd>                   | Delete window and current frame.                                                                                             |
| <kbd>C-x C-5 C-0</kbd>                                          | <kbd>x50</kbd>                  | Delete frame.                                                                                                                |
| <kbd>C-x C-5 C-1</kbd>                                          | <kbd>x51</kbd>                  | Delete all frames except the current one.                                                                                    |
| <kbd>C-x C-5 C-2</kbd>                                          | <kbd>x52</kbd>                  | Create new frame.                                                                                                            |
| <kbd>C-x <f3></kbd> or <kbd>C-! C-p</kbd>                       | <kbd>!p</kbd>                   | Show process list.                                                                                                           |
| <kbd>C-x <f5></kbd> or <kbd>C-! C-k</kbd>                       | <kbd>!k</kbd>                   | Compile using `compile` function.                                                                                            |
| <kbd>C-x <f7></kbd> or <kbd>C-! C-e</kbd>                       | <kbd>!e</kbd>                   | Open Eshell.                                                                                                                 |
| <kbd>C-x <f10></kbd>, <kbd>C-! C-=</kbd> or <kbd>C-! =</kbd>    | <kbd>''=</kbd>                  | Start Ediff between buffers.                                                                                                 |
| <kbd>C-x S-<f10></kbd>, <kbd>C-! C-+</kbd> or <kbd>C-! +</kbd>  | <kbd>''+</kbd>                  | Start Ediff between files.                                                                                                   |
| <kbd>C-x <f11></kbd> or <kbd>C-! C-c</kbd>                      | <kbd>!c</kbd>                   | Show calendar.                                                                                                               |
| <kbd>C-x <C-M-RET></kbd> or <kbd>C-x S-\\</kbd>                  | <kbd>x S-\\</kbd>              | Show init.el file.                                                                                                           |
| <kbd>C-x RET RET</kbd>                                          |                                 | Set rectangular region mark.                                                                                                 |
| <kbd>C-x <f12></kbd> or <kbd>C-! C-n</kbd>                      | <kbd>!n</kbd>                   | Show calculator.                                                                                                             |
| <kbd>C-x ?</kbd>                                                | <kbd>x ?</kbd>                  | Open man page. Available only on GNU/Linux.                                                                                  |
| <kbd>C-x r =</kbd>                                              | <kbd>x r =</kbd>                | Reset highlighted diff (`diff-hl`) in current buffer.                                                                        |
| <kbd>C-<f2></kbd>, <kbd>C-c C-;</kbd> or <kbd>C-c ;</kbd>       | <kbd>c;</kbd>                   | Show `imenu`. Useful for code navigation.                                                                                    |
| <kbd>M-]</kbd> or <kbd>C-[ ]</kbd>                              | <kbd>[ ]</kbd> or <kbd>g]</kbd> | Seach token under cursor (`helm-swoop`).                                                                                     |
| <kbd>C-[ C-[ ]</kbd>                                            | <kbd>[[ ]</kbd>                 | Remove all highlighted tokens.                                                                                               |
| <kbd>C-<f4></kbd>, <kbd>C-' C-l</kbd> or <kbd>C-' l</kbd>       | <kbd>'l</kbd>                   | Toggle highlight current line.                                                                                               |
| <kbd>C-<f6></kbd>, <kbd>C-' C-n</kbd> or <kbd>C-' n</kbd>       | <kbd>'n</kbd>                   | Toggle line numbers.                                                                                                         |
| <kbd>C-<f9></kbd>, <kbd>C-' C-w</kbd> or <kbd>C-' w</kbd>       | <kbd>'w</kbd>                   | Toggle truncate lines (word wrapping).                                                                                       |
| <kbd>C-c (</kbd>                                                | <kbd>c (</kbd>                  | Toggle paredit-mode. Useful for editing Lisp code.                                                                           |
| <kbd>C-' C-' C-c</kbd> or <kbd>C-' ' c</kbd>                    | <kbd>''c</kbd>                  | Toggle camel-case mode.                                                                                                      |
| <kbd>C-S-\\</kbd>                                                | <kbd>S-\\</kbd>                | Move to window.                                                                                                              |
| <kbd>C-+</kbd>                                                  | <kbd>+</kbd>                    | Interactively resize current window.                                                                                         |
| <kbd>C-?</kbd>                                                  | <kbd>?</kbd>                    | Look up any documentation. Changes behaviour depending on major mode, and defaults to available info documentation.          |
| <kbd>C-=</kbd>                                                  | <kbd>=</kbd>                    | Expand region.                                                                                                               |
| <kbd>C-~</kbd>                                                  | <kbd>~</kbd>                    | Show mark ring.                                                                                                              |
| <kbd>``C-` ``</kbd> (backtick)                                  | <kbd>`` ` ``</kbd> (backtick)   | Toggle sticky control key (<kbd>j</kbd>).                                                                                    |
| <kbd><f6></kbd> or <kbd>C-%</kbd>                               | <kbd>%</kbd>                    | Move to matching parentheses.                                                                                                |
| <kbd>C-[ M-x</kbd> or <kbd>C-[ C-[ x</kbd>                      | <kbd>[[ x</kbd>                 | Execute menu command.                                                                                                        |
| <kbd>M-<up></kbd> or <kbd>M-p</kbd>                             | <kbd>gp</kbd>                   | Move line or region up.                                                                                                      |
| <kbd>M-<down></kbd> or <kbd>M-n</kbd>                           | <kbd>gn</kbd>                   | Move line or region down.                                                                                                    |
| <kbd>M-<f5></kbd>, <kbd>C-x C-a C-k</kbd> or <kbd>C-x a k</kbd> | <kbd>xak</kbd>                  | Recompile using `recompile` function.                                                                                        |
| <kbd>M-SPC</kbd>                                                |                                 | Auto-complete symbol at point.                                                                                               |
| <kbd>C-x C-j</kbd>                                              | <kbd>xj</kbd>                   | Show directory.                                                                                                              |
| <kbd>C-c C-j</kbd>                                              | <kbd>cj</kbd>                   | Show directory explorer.                                                                                                     |
| <kbd>C-c C-\\</kbd> or <kbd>C-c \\</kbd>                          | <kbd>c\\</kbd>                | Delete all spaces leaving one space.                                                                                         |
| <kbd>C-' C-' C-a</kbd> or <kbd>C-' ' a</kbd>                    | <kbd>''a</kbd>                  | Toggle auto-completion.                                                                                                      |
| <kbd>C-' C-' C-q</kbd> or <kbd>C-' ' q</kbd>                    | <kbd>''q</kbd>                  | Toggle auto fill mode.                                                                                                       |

### Git integration

| Keys                                                        | God mode Keys                  | Description                             |
| ----------------------------------------------------------- | ------------------------------ | --------------------------------------- |
| <kbd>C-: C-s</kbd>, <kbd>C-: C-:</kbd> or <kbd>C-: :</kbd>  | <kbd>:s</kbd> or <kbd>::</kbd> | Show current git repository status.     |
| <kbd>C-: C-k</kbd>                                          | <kbd>:k</kbd>                  | Run `gitk`.                             |
| <kbd>C-: C-d</kbd>                                          | <kbd>:d</kbd>                  | `git diff` for current file.            |
| <kbd>C-: C-c C-d</kbd>                                      | <kbd>:cd</kbd>                 | `git diff` for current git repository.  |
| <kbd>C-: C-l</kbd>                                          | <kbd>:l</kbd>                  | Show current git repository log.        |
| <kbd>C-: C-f</kbd>                                          | <kbd>:f</kbd>                  | Find file in current git repository.    |
| <kbd>C-: <f10></kbd> or <kbd>C-: C-=</kbd>                  | <kbd>:=</kbd>                  | Start `vc-ediff` for current file.      |
| <kbd>C-: <f3></kbd>, <kbd>C-: M-s</kbd> or <kbd>M-s :</kbd> | <kbd>:gs</kbd>                 | `git grep` in current git repository.   |

### Search

Requires [ag](https://github.com/ggreer/the_silver_searcher).

| Keys                                    | God mode Keys     | Description                     |
| --------------------------------------- | ----------------- | ------------------------------- |
| <kbd>M-s s</kbd> or <kbd>C-<f3></kbd>   | <kbd>gs s</kbd>   | Search in files.                |
| <kbd>M-s r</kbd> or <kbd>C-S-<f3></kbd> | <kbd>gs r</kbd>   | Search regexp in files.         |
| <kbd>M-s a s</kbd>                      | <kbd>gs a s</kbd> | Search in project.              |
| <kbd>M-s a r</kbd>                      | <kbd>gs a r</kbd> | Search regexp in project.       |
| <kbd>M-s o</kbd>                        | <kbd>gs o</kbd>   | Find occurences in buffer.      |
| <kbd>M-s i</kbd>                        | <kbd>gs i</kbd>   | Jump to occurences in buffer.   |
| <kbd>M-s G</kbd>                        | <kbd>gs G</kbd>   | Run grep.                       |
| <kbd>M-s g</kbd>                        | <kbd>gs g</kbd>   | Search using grep.              |

### Bookmarks

| Keys                                                                                                 | God mode Keys  | Description                                  |
| ---------------------------------------------------------------------------------------------------- | -------------  | -------------------------------------------- |
| <kbd>C-c C-: C-;</kbd> or <kbd>C-c : ;</kbd>                                                         | <kbd>c:;</kbd> | Show recently opened files.                  |
| <kbd>C-c C-: C-:</kbd> or <kbd>C-c : :</kbd>                                                         | <kbd>c::</kbd> | Show bookmark list.                          |
| <kbd>C-c C-: C-=</kbd> or <kbd>C-c : =</kbd>                                                         | <kbd>c:=</kbd> | Add bookmark.                                |
| <kbd>C-c C-: C-#</kbd> or <kbd>C-c : #</kbd>                                                         | <kbd>c:#</kbd> | Delete all bookmarks.                        |
| <kbd>C-c C-: C-<left></kbd>, <kbd>C-c C-: C-b</kbd>, <kbd>C-c : <left></kbd> or <kbd>C-c : b</kbd>   | <kbd>c:b</kbd> | Move to previous bookmark.                   |
| <kbd>C-c C-: C-<right></kbd>, <kbd>C-c C-: C-f</kbd>, <kbd>C-c : <right></kbd> or <kbd>C-c : f</kbd> | <kbd>c:f</kbd> | Move to next bookmark.                       |
| <kbd>C-c C-: C-<up></kbd>, <kbd>C-c C-: C-p</kbd>, <kbd>C-c : <up></kbd> or <kbd>C-c : p</kbd>       | <kbd>c:p</kbd> | Move to previous bookmark in current buffer. |
| <kbd>C-c C-: C-<down></kbd>, <kbd>C-c C-: C-n</kbd>, <kbd>C-c : <down></kbd> or <kbd>C-c : n</kbd>   | <kbd>c:n</kbd> | Move to next bookmark in current buffer.     |

### Workgroups

| Keys                          | God mode Keys    | Description                 |
| ----------------------------- | ---------------- | --------------------------  |
| <kbd>C-x C-: C-v</kbd>        | <kbd>x:v</kbd>   | Switch to workgroup.        |
| <kbd>C-x C-: C-a</kbd>        | <kbd>x:a</kbd>   | Switch to next workgroup.   |
| <kbd>C-x C-: C-c</kbd>        | <kbd>x:c</kbd>   | Create a new workgroup.     |
| <kbd>C-x C-: C-k</kbd>        | <kbd>x:k</kbd>   | Kill workgroup.             |
| <kbd>C-x C-: ?</kbd>          | <kbd>x: ?</kbd>  | Show help for workgroups.xs |

### Code folding

| Keys                      | God mode Keys     | Description                            |
| ------------------------- | ----------------- | -------------------------------------- |
| <kbd>C-c h</kbd>          | <kbd>c h</kbd>    | Hide block.                            |
| <kbd>C-c g h</kbd>        | <kbd>c g h</kbd>  | Hide all blocks.                       |
| <kbd>C-c s</kbd>          | <kbd>c s</kbd>    | Show block.                            |
| <kbd>C-c g s</kbd>        | <kbd>c g s</kbd>  | Show all blocks.                       |

### Multiple cursors

| Keys                                        | God mode Keys     | Description                            |
| ------------------------------------------- | ----------------- | -------------------------------------- |
| <kbd>C-x <C-RET></kbd>                      |                   | Edit lines with multiple cursors.      |
| <kbd>C-></kbd>                              | <kbd>></kbd>      | Mark next line or word.                |
| <kbd>C-<</kbd>                              | <kbd><</kbd>      | Mark preivous line or word.            |
| <kbd>C-c C-<f3></kbd> or <kbd>C-c C-></kbd> | <kbd>c></kbd>     | Mark all words like selected word.     |

### org-mode

| Keys             | God mode Keys  | Description              |
| ---------------- | -------------  | ------------------------ |
| <kbd>C-c b</kbd> | <kbd>c b</kbd> | List org-mode buffers.   |

## Mode-specific key bindings

### Ediff

| Keys                                 | Description                          |
| ------------------------------------ | ------------------------------------ |
| <kbd>M-<down></kbd> or <kbd>n</kbd>  | Next diff.                           |
| <kbd>M-<up></kbd> or <kbd>p</kbd>    | Previous diff.                       |
| <kbd>M-<right></kbd> or <kbd>a</kbd> | Move diff from left to right buffer. |
| <kbd>M-<left></kbd> or <kbd>b</kbd>  | Move diff from right to left buffer. |

### dired-mode

| Keys               | Description                            |
| ------------------ | -------------------------------------- |
| <kbd>C-x C-/</kbd> | Switch to editable (wdired-mode).      |

### org-mode

| Keys             | God mode Keys  | Description                                     |
| ---------------- | -------------- | ----------------------------------------------- |
| <kbd>C-x t</kbd> | <kbd>x t</kbd> | Show org-mode timeline.                         |
| <kbd>C-c a</kbd> | <kbd>c a</kbd> | Show org-mode agenda.                           |
| <kbd>C-c c</kbd> | <kbd>c c</kbd> | Capture org-mode template.                      |
| <kbd>C-c l</kbd> | <kbd>c l</kbd> | Store org-mode link.                            |

### Paredit mode

| Keys                                                                     | God mode Keys     | Description                               |
| ------------------------------------------------------------------------ | ----------------- | ----------------------------------------- |
| <kbd>C-[ <right></kbd> or <kbd>C-, C-f</kbd> or <kbd>C-, f</kbd>         | <kbd>,f</kbd>     | Slurp into current sexp from right.       |
| <kbd>C-[ <left></kbd> or <kbd>C-, C-b</kbd> or <kbd>C-, b</kbd>          | <kbd>,b</kbd>     | Barf from current sexp from right.        |
| <kbd>C-[ C-[ <right></kbd>, <kbd>C-, C-, C-f</kbd> or <kbd>C-, , f</kbd> | <kbd>,,f</kbd>    | Barf from current sexp from left.         |
| <kbd>C-[ C-[ <left></kbd>, <kbd>C-, C-, C-b</kbd> or <kbd>C-, , b</kbd>  | <kbd>,,b</kbd>    | Slurp into current sexp from left.        |
| <kbd>C-[ C-[ <down></kbd>, <kbd>C-, C-k</kbd> or <kbd>C-, k</kbd>        | <kbd>,k</kbd>     | Splice current sexp by killing forward.   |
| <kbd>C-[ C-[ <up></kbd>, <kbd>C-, C-, C-k</kbd> or <kbd>C-, , k</kbd>    | <kbd>,,k</kbd>    | Splice current sexp by killing backward.  |

### All Lisp modes

Available in Clojure, Scheme and Emacs Lisp modes.

| Keys                                     | God mode Keys | Description                              |
| ---------------------------------------- | ------------- | ---------------------------------------- |
| <kbd>C-x C-e</kbd>                       | <kbd>xe</kbd> | Evaluate last sexp.                      |
| <kbd>C-c C-k</kbd>                       | <kbd>ck</kbd> | Evaluate current buffer.                 |
| <kbd>C-c C-l</kbd>                       | <kbd>cl</kbd> | Load file.                               |
| <kbd>C-<f12></kbd> or <kbd>C-' C-s</kbd> | <kbd>'s</kbd> | Toggle highlight s-expresion.            |

### GUD mode

Requires GUD debugger to be active.
Supports `gdb` for C, `pdb` for Python, `ruby -r debug` for Ruby and XDebug for PHP.

| Keys                                                                | God mode Keys        | Description                              |
| ------------------------------------------------------------------- | -------------------- | ---------------------------------------- |
| <kbd><f5></kbd>, <kbd>C-x C-a C-r</kbd> or <kbd>C-x a r</kbd>       | <kbd>xar</kbd>       | Resume or continue execution.            |
| <kbd><f8></kbd>, <kbd>C-x C-a C-x C-r</kbd> or <kbd>C-x a x r</kbd> | <kbd>xaxr</kbd>      | Debug program/script.                    |
| <kbd><f9></kbd>, <kbd>C-x C-a C-b</kbd> or <kbd>C-x a b</kbd>       | <kbd>xab</kbd>       | Set breakpoint.                          |
| <kbd>S-<f9></kbd>, <kbd>C-x C-a C-d</kbd> or <kbd>C-x a d</kbd>     | <kbd>xad</kbd>       | Delete breakpoint.                       |
| <kbd><f10></kbd>, <kbd>C-x C-a C-n</kbd> or <kbd>C-x a n</kbd>      | <kbd>xan</kbd>       | Next statement.                          |
| <kbd><f11></kbd>, <kbd>C-x C-a C-s</kbd> or <kbd>C-x a s</kbd>      | <kbd>xas</kbd>       | Step into statement.                     |
| <kbd>S-<f11></kbd>, <kbd>C-x C-a C-f</kbd> or <kbd>C-x a f</kbd>    | <kbd>xaf</kbd>       | Finish execution of current function.    |

### Clojure

Requires [Leiningen](https://github.com/technomancy/leiningen).

| Keys                                                            | God mode Keys        | Description                                    |
| --------------------------------------------------------------- | -------------------- | ---------------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd>       | Load and switch to current namespace in nREPL. |
| <kbd>C-<f8></kbd> or <kbd>C-! C-o</kbd>                         | <kbd>!o</kbd>        | Connect to nREPL server.                       |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>        | Start nREPL server and connect.                |
| <kbd>C-?</kbd>                                                  | <kbd>?</kbd>         | Show nREPL documentation for current word.     |
| <kbd>C-x T</kbd>                                                | <kbd>x T</kbd>       | Run all tests for current namespace.           |
| <kbd>C-x t</kbd>                                                | <kbd>x t</kbd>       | Run current test.                              |

### Emacs Lisp

| Keys                                                            | God mode Keys        | Description                                                                                          |
| --------------------------------------------------------------- | -------------------- | ---------------------------------------------------------------------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd>       | Evaluate current buffer. When used in the `*scratch*` buffer, it evaluates and prints the last sexp. |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>        | Open Eshell.                                                                                         |


### Scheme

Requires Racket and/or Guile Scheme.

| Keys                                                            | God mode Keys        | Description                           |
| --------------------------------------------------------------- | -------------------- | ------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd>       | Evaluate current buffer in REPL.      |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>        | Start Scheme REPL server and connect. |


<h3> C# </h3>

Requires [Omnisharp](https://github.com/nosami/Omnisharp).

| Keys                                      | God mode Keys        | Description                                             |
| ----------------------------------------- | -------------------- | ------------------------------------------------------- |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>  | <kbd>!r</kbd>        | Start Omnisharp server.                                 |
| <kbd>C-x <f5></kbd> or <kbd>C-! C-k</kbd> | <kbd>!k</kbd>        | Build current solution.                                 |
| <kbd>C-?</kbd>                            | <kbd>?</kbd>         | Show overloads of symbol at point.                      |
| <kbd><f12></kbd> or <kbd>M-.</kbd>        | <kbd>g.</kbd>        | Go to definition.                                       |
| <kbd>S-<f12></kbd> or <kbd>C-[ M-.</kbd>  | <kbd>[g.</kbd>       | Find all references.                                    |
| <kbd>C-c F</kbd>                          | <kbd>c F</kbd>       | Format document.                                        |


### JavaScript

Requires Node.js, [swank-js](https://github.com/swank-js/swank-js) and [tern](https://github.com/marijnh/tern).

| Keys                                                            | God mode Keys  | Description                                 |
| --------------------------------------------------------------- | -------------  | ------------------------------------------- |
| <kbd>C-x C-e</kbd>                                              | <kbd>xe</kbd>  | Evaluate current statement.                 |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd> | Evaluate current buffer and switch to REPL. |
| <kbd>C-<f8></kbd> or <kbd>C-! C-o</kbd>                         | <kbd>!o</kbd>  | Connect to SwankJS sever.                   |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>  | Start SwankJS REPL.                         |

### Haskell

Requires Haskell platform and `haskell-doc`.

| Keys                                                            | God mode Keys  | Description                                 |
| --------------------------------------------------------------- | -------------  | ------------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd> | Evaluate current buffer and switch to REPL. |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>  | Start GHCi REPL.                            |

### PHP

Requires XDebug PHP extension.

| Keys                                                            | God mode Keys  | Description                             |
| --------------------------------------------------------------- | -------------- | --------------------------------------- |
| <kbd>C-<f11></kbd> or <kbd>C-! C-d</kbd>                        | <kbd>!d</kbd>  | Start PHP debugger.                     |

### Ruby

Requires Ruby and `rake`.

| Keys                                                            | God mode Keys  | Description                             |
| --------------------------------------------------------------- | -------------- | --------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd> | Evaluate current buffer in REPL.        |
| <kbd>C-<f8></kbd> or <kbd>C-! C-a</kbd>                         | <kbd>!a</kbd>  | Run Rake task.                          |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>  | Start irb REPL.                         |
| <kbd>C-<f11></kbd> or <kbd>C-! C-d</kbd>                        | <kbd>!d</kbd>  | Start Ruby debugger.                    |

### Python

| Keys                                                            | God mode Keys  | Description                             |
| --------------------------------------------------------------- | -------------- | --------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>xaa</kbd> | Evaluate current buffer in REPL.        |
| <kbd>C-<f10></kbd> or <kbd>C-! C-r</kbd>                        | <kbd>!r</kbd>  | Start Python REPL.                      |
| <kbd>C-<f11></kbd> or <kbd>C-! C-d</kbd>                        | <kbd>!d</kbd>  | Start PDB debugger.                     |
| <kbd>C-?</kbd>                                                  | <kbd>?</kbd>   | Show ri documentation for current word. |
| <kbd>C-x T</kbd>                                                | <kbd>x T</kbd> | Run all tests via `rake test`.          |

### gnuplot

Requires gnuplot.

| Keys                                                                                | God mode Keys                   | Description                             |
| ----------------------------------------------------------------------------------- | ------------------------------- | --------------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-c C-k</kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd> | <kbd>ck</kbd> or <kbd>xaa</kbd> | Evaluate current buffer in REPL.        |
| <kbd>C-<f10></kbd>, <kbd>C-c C-z</kbd> or <kbd>C-! C-r</kbd>                        | <kbd>cz</kbd> or <kbd>!r</kbd>  | Show/start gnuplot REPL.                |

### C

Available only on GNU/Linux.

| Keys                                                             | God mode Keys                  | Description                     |
| ---------------------------------------------------------------- | ------------------------------ | ------------------------------- |
| <kbd>C-<f5></kbd>, <kbd>C-x C-a C-a</kbd> or <kbd>C-x a a</kbd>  | <kbd>xaa</kbd>                 | Run program in <kbd>gdb</kbd>.  |
| <kbd>C-<f10></kbd>, <kbd>C-! C-r</kbd> or <kbd>C-c C-z</kbd>     | <kbd>!r</kbd> or <kbd>cz</kbd> | Switch to `gdb` console buffer. |
| <kbd>C-<f11></kbd> or <kbd>C-! C-d</kbd>                         | <kbd>!d</kbd>                  | Start `gdb`.                    |
| <kbd>C-<f12></kbd>, <kbd>C-x C-a C-q</kbd> or <kbd>C-x a q</kbd> | <kbd>xaq</kbd>                 | Display disassembly.            |

### HTML

| Keys              | Description                   |
| ----------------- | ----------------------------- |
| <kbd>M-SPC</kbd>  | Zencoding expand line.        |
