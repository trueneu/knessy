# Architecture

## Overview

A Knessy buffer is a `tabulated-list-mode` buffer (`knessy-mode`).
What it shows is fully described by its env, see below.
Refreshing runs `kubectl` for the current env, parses the output into items, and repaints the table.

## Env

`knessy--env` (in `knessy-env.el`) returns a list of eight elements:

1. `knessy-kubeconfig`
2. `knessy--context`
3. `knessy--namespace`
4. `knessy--resource-type`
5. `knessy--label-selectors`
6. `knessy--field-selectors`
7. `knessy--view`
8. `knessy--linked-root`

Envs are compared with `equal`.
Every call of `knessy--display2` pushes the current env on a per-buffer ring (`knessy--env-ring`, 32 entries).
`knessy-env-go-back`, `knessy-env-go-forward` and `knessy-env-history` walk that ring.
`knessy--set-env` applies an env to the current buffer, but does not redisplay.
When adding a new piece of state that changes what the buffer shows, add it to the env, otherwise history navigation will lose it.

## Resource types

Real types come from `kubectl api-resources`.
Three synthetic types exist, with names set by customization variables:

- `*ALL-NS*` (`knessy-all-namespaced-resource-types-string`): all namespaced types.
- `*ALL-CLUSTER*` (`knessy-all-cluster-resource-types-string`): all cluster-scoped types.
- `*LINKED*` (`knessy-linked-resource-type-string`): resources linked to one root resource.

`knessy--kubectl-cmd` signals an error when asked to run with a synthetic type.
They only work through a view that expands them.

## Views

`knessy-views` (in `knessy-views.el`) is a hash table keyed by `(resource-type . view-name)`.
A view is an alist with, among others, `:calls`, `:columns`, `:column-rename`, `:widths`, `:pre-process` and `:post-process-item`.
Without an entry, the default view is a single plain `kubectl get`.
`knessy-default-view-alist` picks the default view name per resource type (pods default to `concise`).
Several calls in one view are merged by item key with `knessy--merge-items`, for example `get` joined with `top`.

## Display pipeline

1. `knessy--display2` echoes a message, pushes the env, and dispatches.
2. For `*LINKED*` it calls `knessy--display-linked`.
3. Otherwise it runs the view calls, synchronously (`knessy--display2-sync`) or asynchronously (`knessy--display2-aio`).
4. `knessy--display2-common-part` merges results, post-processes items, adds a NAMESPACE column when the namespace is all, and stores everything in `knessy--data`.
5. `knessy--repaint` renders `knessy--data` into the tabulated list.

`knessy--refresh-in-progress` guards against overlapping refreshes.
Setting `knessy--debug` to non-nil disables that guard.

## Linked view

Described in the header of `knessy-linked.el`.
In short: one `kubectl get <many types> -o json` call fetches the whole namespace, and the graph is walked in memory.
The walk is directional on purpose: ancestors, descendants, forward references, then incoming references.
Following all edges both ways would link a pod to every pod of its service and to the whole cluster through its node.
Only cluster-scoped objects (nodes, PVs, ...) are fetched one by one, on demand.
Events and metrics types are ignored, see `knessy-linked-ignored-resource-types`.

## Processes

`knessy-process.el` has the primitives.
`knessy--shell-exec` runs a command synchronously.
`knessy--shell-exec-async2` and `knessy--shell-exec-async3` run commands in the background with a filter and a callback sentinel.
There is a TODO to move everything to the `async3` variant.
Kubectl commands are built as shell strings by `knessy--kubectl-cmd`, so values in them are not quoted automatically.
The newer code in `knessy-exec.el` and `knessy-linked.el` passes argument lists instead; prefer that for new code.

## Cache

`knessy-cache.el` is a TTL cache of nested hash tables, default TTL one day.
Labels are cached per context, namespace and type, with a shorter TTL (`knessy-label-cache-ttl`, 120 seconds).
