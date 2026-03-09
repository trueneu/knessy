# Knessy

Knessy is an Emacs Kubernetes management package.

It is, like any other Emacs package, written in Emacs Lisp.

## Project structure

All the code lives in top-level `.el` files, roughly categorized by purpose.
`knessy.el` is the main entrypoint.

Before starting work, read `CONTEXT.org` in the repository root (conventions, decisions, file map).
Longer design notes and pitfalls live in `agent-notes/`: `architecture.md`, `shell-and-debug.md` and `gotchas.md`.
Record new decisions and findings in `CONTEXT.org`.

## Idea

The main idea of the project is to take inspiration from a highly successful `k9s` project in Emacs buffers, while taking
advantage of Emacs programmability. The central point is resources discoverability, which is achieved with using Emacs standard
`tabulated-list-mode` to represent information, roughly like `kubectl get` does, but in interactive manner.

## Goal

End goal is to be fast and useful for tasks involving debugging and troubleshooting kubernetes clusters.
