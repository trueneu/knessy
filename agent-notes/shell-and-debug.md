# Shell and debug container

Both live under the `knessy-shell` transient (key `s` in a Knessy buffer), implemented in `knessy.el` and `knessy-exec.el`.

## Transient

- `-c` / `--container=`: container name.
  The reader `knessy--shell-read-container` asks `kubectl` for the container names of the pod at point.
  It does not ask when the pod has one container, and signals a `user-error` when it has none.
- `s`: `knessy-shell-into`, a shell inside the pod.
- `d`: `knessy-shell-debug`, a debug container in the pod.

Both actions signal a `user-error` when the resource at point is not a pod.
Both only use the first selected id, even when several are marked (there is a TODO for that).

## Shell into a pod

`knessy-exec-pod` registers a TRAMP method that runs `kubectl exec -it POD [-c CONTAINER] -- sh`.
It then opens vterm or ghostel in a TRAMP directory like `/METHOD:POD:/`, so the terminal runs `/bin/sh` inside the pod.
The method name embeds a hash of context, namespace and container (`knessy--tramp-method-name`), because TRAMP caches connections by method and host.
Without the hash, a second shell in another cluster or container would reuse the first connection.
When `default-directory` is already remote, `knessy--dir-prefix` adds a hop prefix so the pod is reached through that connection.
This code was taken from kubel.

## Debug container

`knessy-debug-pod` runs

```
kubectl --kubeconfig KUBECONFIG --context CTX --namespace NS debug -it POD --image ubuntu:latest [--target CONTAINER] -- /bin/bash
```

in a terminal, with the image taken from the string option `knessy-debug-image` and the shell entry point from `knessy-debug-shell`.
The defaults are `ubuntu:latest` and `/bin/bash`; images without Bash can use `/bin/sh` by customizing `knessy-debug-shell`.
`kubectl debug` adds an ephemeral container to the pod.
Ephemeral containers cannot be removed or changed afterwards, they live until the pod is gone.
The user needs permission to patch `pods/ephemeralcontainers`.

The container chosen with `-c` becomes `--target`, so the debug container shares that container's process namespace.
Since the reader auto-picks the only container, `--target` is normally always set.
Targeting needs runtime support; if a cluster rejects it, drop `--target` for that case.

This command does not use TRAMP.
`kubectl` is started locally by the terminal, so `default-directory` is forced to a local directory (`~/`) when the current one is remote.
The buffer is named `*TERMINAL_CTX_NS_POD_debug`, made unique with `generate-new-buffer-name`.

## Passing the command to the terminal

The two terminals take the command differently:

- ghostel: `ghostel-shell` is a program path, or a list of program and arguments.
  A single string with spaces is treated as the program path and fails with "Searching for program: No such file or directory".
  This bit us once, and was verified by the user.
- vterm: `vterm-shell` is a string.
  The code passes `combine-and-quote-strings` of the command list.
  This path was not exercised by the user, only ghostel was.

So `knessy--debug-command` returns a list, and `knessy-debug-pod` converts it for vterm.
`knessy--exec-vterm` and `knessy--exec-ghostel` take an optional shell argument that defaults to `/bin/sh`.

## Ideas not done

- Make the image configurable per call, or a `defcustom`, if the user asks for it.
  It is a constant on purpose for now.
- Support multiple marked pods, one terminal each.
- Offer a transient option for the debug image or `--copy-to` (debugging a copy of the pod).
