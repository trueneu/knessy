# Knessy

A k9s-style Kubernetes interface for Emacs.
Heavily inspired by abrochard's kubel.

## What it is for

Knessy shows Kubernetes resources in a `tabulated-list-mode` buffer, roughly like `kubectl get` does, but interactive.
You can switch context, namespace and resource type, filter and sort, and act on resources without leaving Emacs.
It is built to be fast and useful for debugging and troubleshooting clusters.

## Who benefits

- SREs and developers who investigate and debug Kubernetes workloads day to day.
- Emacs users who like keyboard-driven, buffer-based tools and want a k9s-like experience inside Emacs.
- Anyone juggling several clusters and namespaces who wants to jump between them quickly.

## Installation

Requirements:

- Emacs 28.1 or newer
- `kubectl` available in `PATH`
- Emacs packages: `s`, `dash`, `ht`, `asoc`, `transient`
- Optional: `vterm` (for shelling into pods), `embark` (extra actions on namespaces)

Clone the repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/knessy")
(require 'knessy)
```

Then run `M-x knessy`.
Use `M-x knessy-new` to open an additional Knessy buffer.
Knessy respects `KUBECONFIG`.

## Core functions

Inside a Knessy buffer, the main keys are:

| Key | Command | What it does |
|-----|---------|--------------|
| `c` | `knessy-config` | Pick resource type, namespace, context, kubeconfig file or view |
| `f` | `knessy-filter` | Filter by label, field selector or regex |
| `j` | `knessy-jump` | Jump to children, linked resources, owner, node, node pods or namespace |
| `d` / `e` | `knessy-describe` / `knessy-edit` | Describe or edit the resource (YAML, JSON with prefix arg) |
| `l` | `knessy-log` | Show logs, with follow, tail and container options |
| `s` | `knessy-shell` | Shell into a pod, choosing the container with `-c` (terminal set by `knessy-shell-terminal`: vterm or ghostel) |
| `F` | `knessy-port-forward` | Start port forwarding |
| `k` | `knessy-kill` | Delete resources, with cascade and grace period options |
| `m` / `u` | `knessy-mark` / `knessy-unmark` | Mark resources to act on several at once (`M` / `U` for all) |
| `[` / `]` / `z` | `knessy-env-go-back` / `-forward` / `-history` | Move through your history of environments (context, namespace, resource type, filters) |
| `b` / `K` | `knessy-switch-buffer` / `knessy-cleanup-buffers` | Switch between and clean up Knessy buffers |

Also available: `knessy-apply-arbitrary` and `knessy-delete-arbitrary` to apply or delete manifests from files.
