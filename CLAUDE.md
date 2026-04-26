# Knessy

Claude, you're working on the most advanced kubernetes mode for Emacs ever!

It is, as any other Emacs package, written in Emacs Lisp.

I am a very inexperienced Emacs Lisp programmer, so I might make silly mistakes.

## Project structure

All the code lives in `.el` files, roughly categorized by purpose.

- `knessy.el` is the main entrypoint.
- `knessy-cache.el`  works with caches.
- `knessy-comparators.el` defines comparator functions.
- `knessy-kubectl.el` is an interface with `kubectl` CLI.
- `knessy-process.el` defines functions to work with processes.
- `knessy-representation.el` works with data representations.
- `knessy-tests.el` is largely unused now.
- `knessy-units.el` works with unit conversions.
- `knessy-utils.el` defines helper functions.
- `knessy-views.el` defines different "views" (custom table configurations).

## Idea

The main idea of the project is to mimic highly successful `k9s` project in Emacs buffers. The central point is discoverability,
which is achieved with using Emacs standard `tabulated-list-mode` to represent information, roughly like `kubectl get` does, but
in interactive manner.

## Goal

End goal is to be fast and useful for tasks involving debugging and troubleshooting kubernetes clusters at my workplace.

## Testing

For testing, you may use context k8s-local, with KUBECONFIG defined at ~/.kube/k8s-local. Assume the cluster has absolutely no workloads running but the bare minimum required for kubernetes (it is a k3s cluster).
