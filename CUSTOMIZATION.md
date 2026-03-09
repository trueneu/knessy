## Customization

All options live in the `knessy` customization group.

### Kubernetes access

| Option | Default | What it does |
|--------|---------|--------------|
| `knessy-kubectl` | `"kubectl"` | Path to the `kubectl` binary. Point it at a specific build or a wrapper script. |
| `knessy-kubeconfig` | `$KUBECONFIG` | Kubeconfig path. Do not use several files separated by `:`. Switch it on the fly with `c f`. |
| `knessy-initial-context` | `""` | Context for the first Knessy buffer. Empty means the current `kubectl` context. |
| `knessy-initial-namespace` | `"kube-system"` | Namespace for the first Knessy buffer. |
| `knessy-initial-resource-type` | `"pods"` | Resource type for the first Knessy buffer. |

### Filters

Label and field selectors survive changes of context, namespace and resource type by default.
These options, all defaulting to `nil`, make Knessy reset them on a change:

- `knessy-reset-label-selectors-on-context-change`, `knessy-reset-field-selectors-on-context-change`
- `knessy-reset-label-selectors-on-namespace-change`, `knessy-reset-field-selectors-on-namespace-change`
- `knessy-reset-label-selectors-on-resource-type-change`, `knessy-reset-field-selectors-on-resource-type-change`

### Shell and debug containers

| Option | Default | What it does |
|--------|---------|--------------|
| `knessy-shell-terminal` | `vterm` | Terminal package used by `s`: `vterm` or `ghostel`.  |
| `knessy-debug-image` | `"ubuntu:latest"` | Debug container image |
| `knessy-debug-shell` | `"/bin/bash"` | Debug container shell. |

### Display

| Option | Default | What it does |
|--------|---------|--------------|
| `knessy-use-short-resource-type-names` | `t` | Show short resource type names such as `deploy`. |
| `knessy-resource-type-short-names` | `nil` | Alist of full resource type name to the short name to show, e.g. `'(("deployments.apps" . "dep"))`. |
| `knessy-default-column-width` | `16` | Width of columns that have no entry in `knessy-column-widths`. |
| `knessy-column-widths` | hash table | Column widths by column name. |
| `knessy-views` | hash table | Per-resource-type views: which `kubectl` calls to make, which columns to show, their widths and post-processing. |
| `knessy-default-view-alist` | `(("pods" . "concise"))` | View used by default for a resource type. |
| `knessy-usage-percentage-warning`, `knessy-usage-percentage-critical` | `80`, `90` | Usage percentages at which CPU and memory values change color. |
| `knessy-display-start-messages` | a few messages | Messages shown while a buffer is loading. |
| `knessy-base-buffer-name` | `"knessy"` | Base name of Knessy buffers. |

### The linked view

The `*LINKED*` view lists the resources related to one resource -- which takes many `kubectl` calls.

| Option | Default | What it does |
|--------|---------|--------------|
| `knessy-linked-parallelism` | `8` | How many `kubectl` calls run at once. |
| `knessy-linked-types-per-call` | `10` | How many resource types one call lists. |
| `knessy-linked-heavy-resource-types` | `("pods")` | Resource types always listed in a call of their own. |
| `knessy-linked-ignored-resource-types` | see the docstring | Resource types left out of the view. |

### Caching

| Option | Default | What it does |
|--------|---------|--------------|
| `knessy-cache-ttl-default` | one day | How long cached data such as resource types lives, in seconds. |

### Provider pages for nodes

`j P` (`knessy-open-node-in-provider`) opens the page of a node in a browser.
On a pod it opens the page of the node the pod runs on.
AWS EC2, Azure and Alibaba ECS nodes need no configuration, the link is built from the node's `spec.providerID`.

Bare metal nodes may have no `providerID`, so Knessy needs to know which clusters are metal and where their inventory page is:

| Option | Default | What it does |
|--------|---------|--------------|
| `knessy-metal-context-regexp` | `nil` | Regexp matching the context names of your bare metal clusters. |
| `knessy-metal-provider-id` | `nil` | The `spec.providerID` your metal nodes have. `nil` means the field is missing. Set it to a string, for example `"private"`, if your environment puts a fixed marker there. |
| `knessy-metal-node-url-format` | `nil` | URL of a node's inventory page, with `%s` standing for the node name. |

The metal provider is off until the regexp and the URL format are set.
It looks at nodes of the matching clusters that have no `providerID`.
If yours have one, such as `private`, set `knessy-metal-provider-id` to it, and only nodes with exactly that `providerID` are treated as metal.
For example:

```elisp
(setopt knessy-metal-context-regexp "\\`dc1-"
        knessy-metal-node-url-format "https://inventory.example.com/hosts/%s")
```

Anything else can be added with `knessy-node-url-functions`, a list of functions called with the node (parsed JSON as nested hash tables) and the context name.
Each returns a URL or `nil`:

```elisp
(add-hook 'knessy-node-url-functions
          (lambda (node context)
            (when (string-prefix-p "lab-" context)
              (format "https://lab.example.com/%s" (ht-get* node "metadata" "name")))))
```
