# Contributing

## Emacs Lisp conventions

Definitions in files within the `core/` directory should use the following prefixes:

| Prefix           | Description                                                     |
| :--              | :--                                                             |
| `holy-emacs-...` | Customizable variables and faces                                |
| `core/...`       | Interactive functions                                           |
| `core:...`       | Public non-interactive functions                                |
| `core--...`      | Any non-interactive private definitions                         |
| `core-...`       | Any other public definitions like `defvar` and `defconst` forms |

Definitions in files outside the `core/` directory should have the `+pkg` prefix, depending on
which package or feature (`pkg`) they are used for, and use similar conventions:

| Prefix      | Description                                                                  |
| :--         | :--                                                                          |
| `+pkg/...`  | Interactive functions                                                        |
| `+pkg:...`  | Public non-interactive functions                                             |
| `+pkg--...` | Any non-interactive private definitions                                      |
| `+pkg-...`  | Any other public definitions like `defcustom`, `defvar` and `defconst` forms |

The only exceptions to these conventions are:

* The `holy-emacs` customization group (`core.el`)
* The `holy-emacs-version` constant (`core.el`)

## `use-package` conventions

Declare `use-package` keyword arguments in the following order:
* `:straight`
* `:if`
* `:unless`
* `:defer`
* `:after`
* `:load-path`
* `:mode`
* `:commands`
* `:hook`
* `:lang`
* `:bind`
* `:init`
* `:config`
* `:catch`
* Other keywords

Do not use the `:ensure` keyword argument with `use-package` as `straight` package manager is used.

## Key binding conventions

Define key bindings following these conventions:

* Avoid using the <kbd>C-x</kbd> prefix for mode specific key bindings.
* Avoid using the <kbd>C-c C-</kbd> prefix for global key bindings.
* Use the <kbd>C-'</kbd> and <kbd>C-' C-</kbd> prefixes for toggling minor modes.
* Use <kbd>ESC</kbd> as a prefix but do not rebind it.
* Do not rebind <kbd>C-g</kbd> or <kbd>C-h</kbd>.

Note that these conventions are based on the documented [Emacs key binding conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html).
