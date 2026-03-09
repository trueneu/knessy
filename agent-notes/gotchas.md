# Gotchas

## defcustom and defvar do not reload

`defcustom` and `defvar` leave an already-bound variable alone.
So `load-file` or `eval-buffer` of `knessy-views.el` does not update `knessy-views` in a live session.
Restart Emacs, or `(makunbound 'knessy-views)` and reload.
The same applies to `knessy-shell-terminal` and other options, but not to `defconst` (it always resets) or `defun`.
`knessy--kubectl-cmd` even has an error message about this.

## Terminal programs take lists, not command lines

ghostel's `ghostel-shell` treats a string as a program path.
Pass a list of program and arguments for commands with arguments.
See `shell-and-debug.md`.

## Optional packages and byte-compiling

vterm and ghostel are optional.
Their variables are declared with `defvar` (without a value) at the top of `knessy-exec.el`, so `let` bindings of them are dynamic even when they are not loaded.
Do not remove these declarations.
Their functions are declared with `declare-function` and the packages are `require`d on first use.

## Batch byte-compile

`emacs --batch -f batch-byte-compile` fails with "Cannot open load file: s" unless the package directories are on the load path.
For a quick syntax check, read all forms of a file instead.

## kubectl command strings

`knessy--kubectl-cmd` and the `knessy--shell-exec*` family use shell strings.
`knessy-kubectl` may contain arguments, so use `split-string-and-unquote` when building argument lists.
When passing user-controlled values (names, selectors) prefer argument lists with `process-lines` or `make-process`.

## Buffer-local state

`knessy--context`, `knessy--namespace`, `knessy--resource-type` and friends are buffer-local.
Callbacks of async processes run in another buffer, so capture the values (or the buffer) before starting the process.
Changing the major mode resets buffer-locals; `knessy--logs` re-applies the env after `knessy-log-mode` for this reason.

## Transient arguments

`transient-args` returns the arguments of a prefix; `transient-arg-value` extracts one by its `--name=` key.
A reader function gets `(PROMPT INITIAL-INPUT HISTORY)`.
When an action is invoked from a transient, `interactive` must call `(transient-args 'PREFIX)`.

## Reloading in a live session

`knessy.el` only `require`s the other files, and `require` does nothing for a feature that is already loaded.
So `load-file` of `knessy.el` does not reload the other files; `load-file` each edited file instead.

## Byte-compiling

Byte-compiling evaluates every `require` at the top of a file, which loads the required file's source.
So an option that another file uses while it loads (in a `defcustom` default, for example) has to be defined in a file that does not require that file back.
Those options live in `knessy-options.el`; this used to fail with a void variable error when they were in `knessy.el`.
Put any new option of this kind there too.
