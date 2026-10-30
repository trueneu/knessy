;;; TODO - write a proper emacs package header  -*- lexical-binding: t; -*-

;; TODO: reorganize all the code and fix the names this is unbearable, come up with a sane naming scheme

;; dependencies: emacs 28.1, s.el, transient, dash, asoc, ht

(require 's)
(require 'asoc)
(require 'dash)

(defvar knessy--log-level 1
  "Takes values from 0 to 5. 5 means all logging.")

(defvar knessy--debug t
  "Set to T to turn off some safety-net features.")

(defun knessy--log (level obj)
  "Print OBJ to `standard-output' if logging is verbose enough for LEVEL.
LEVEL is a number from 0 to 5.
Nothing is printed unless `knessy--log-level' is at least LEVEL.
OBJ is printed with `princ'; if it is not a string, a newline is
printed after it.
Return value is not meaningful."
  (when (>= knessy--log-level level)
    (princ obj)
    (unless (stringp obj)
      (princ "\n"))))

(defcustom knessy-default-view-string "default"
  "String depicting default view for a resource-type."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-namespaced-resource-types-string "*ALL-NS*"
  "String depicting all namespaced resource types.

When the resource-type is set to this value, Knessy queries every
namespaced resource type known to the cluster and shows namespace,
name and kind for each one, instead of the columns of a single
resource type."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-cluster-resource-types-string "*ALL-CLUSTER*"
  "String depicting all cluster-scoped (non-namespaced) resource types.

When the resource-type is set to this value, Knessy queries every
cluster-scoped resource type known to the cluster and shows name and
kind for each one (there's no NAMESPACE column, since it would always
be empty), instead of the columns of a single resource type."
  :type 'string
  :group 'knessy)

(defcustom knessy-linked-resource-type-string "*LINKED*"
  "String depicting the pseudo resource-type of the linked resources view.

When the resource-type is set to this value, Knessy lists the resources
linked (by ownership or references) to `knessy--linked-root', instead
of the resources of a single type."
  :type 'string
  :group 'knessy)

;; should all be requires here

(load "./knessy-kubectl.el")
(load "./knessy-exec.el")
(load "./knessy-process.el")
(load "./knessy-representation.el")
(load "./knessy-units.el")
(load "./knessy-utils.el")
(load "./knessy-comparators.el")
(load "./knessy-views.el")
(load "./knessy-cache.el")
(load "./knessy-linked.el")
(load "./knessy-env.el")

(require 'knessy-kubectl)
(require 'knessy-exec)
(require 'knessy-process)
(require 'knessy-representation)
(require 'knessy-units)
(require 'knessy-utils)
(require 'knessy-comparators)
(require 'knessy-views)
(require 'knessy-cache)
(require 'knessy-linked)
(require 'knessy-env)
(require 'knessy-port-forward)

(load "./knessy-tests.el")
(require 'knessy-tests)

(defgroup knessy nil "Customisation group for Knessy."
  :group 'extensions)

(defcustom knessy-kubeconfig (getenv "KUBECONFIG")
  "Kubectl config(s) path. May contain multiple files delimited by colon (`:`)"
  :type 'string
  :group 'knessy)

;; (defcustom knessy-kubectl "/usr/bin/kubectl"
;;   "Kubectl path. Override to use a nigthly build or a wrapper script."
;;   :type 'string
;;   :group 'knessy)

(defcustom knessy-kubectl "/home/pgu/bin/kubectl"
  "Kubectl path. Override to use a nigthly build or a wrapper script."
  :type 'string
  :group 'knessy)

(defcustom knessy-base-buffer-name "knessy"
  "Default buffer name for Knessy."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-namespaces-string "*ALL*"
  "String depicting all namespaces."
  :type 'string
  :group 'knessy)

;; TODO: unused! use it!
(defcustom knessy-restarts-sorting 'time
  "Sorting function for RESTARTS column. Can be sorted by either
time of the last restart, or the amount of restarts."
  :type '(choice (const :tag "By time" time)
                 (const :tag "By restarts" restarts))
  :group 'knessy)

(defun knessy--reset-in-progress ()
  "Clear the refresh-in-progress flag of the current buffer.
Set the buffer-local `knessy--refresh-in-progress' to nil, so the next
refresh is not skipped.
Use this to recover when a refresh got stuck and the table does not
update any more."
  (interactive)
  (setq knessy--refresh-in-progress nil))

(defun knessy-env-go-back ()
  "Switch the current buffer to the previous env in the env ring.
Move the read position of the ring one entry back with
`knessy--env-ring-read-back' and apply that env with `knessy--set-env'.
Stay on the same env if already at the oldest one.
This only sets the buffer-local variables of the env; it does not
redisplay anything."
  (interactive)
  (let ((env (knessy--env-ring-read-back)))
    (knessy--set-env env)))

(defun knessy-env-go-forward ()
  "Switch the current buffer to the next env in the env ring.
Move the read position of the ring one entry forward with
`knessy--env-ring-read-forward' and apply that env with
`knessy--set-env'.
Stay on the same env if already at the newest one.
This only sets the buffer-local variables of the env; it does not
redisplay anything."
  (interactive)
  (let ((env (knessy--env-ring-read-forward)))
    (knessy--set-env env)))

(transient-define-prefix knessy-log ()
    "Transient menu to show logs of the selected resource(s).

The options are passed to kubectl logs: `-f' follows the logs, `-a'
includes all containers, `-t' sets the number of lines to start from,
`-p' prints the pod and container prefix and `-m' prints timestamps.
By default the last 100 lines are shown and followed.
The `l' action runs `knessy--logs'."
    :value '("--tail=100" "--follow")
    ["Options"
     ("-f" "Follow logs"          "--follow")
     ("-a" "All containers"       "--all-containers")
     ("-t" "Tail (lines)"         "--tail="     :reader transient-read-number-N+)
     ("-p" "Print pod/container prefix" "--prefix")
     ("-m" "Print timestamps"     "--timestamps")]
    ["Actions"
     ("l" "Display logs" knessy--logs)])

(transient-define-argument knessy-kill:--cascade ()
  "Transient option `--cascade=' of `knessy-kill'.
The value is the deletion cascade strategy, read from the choices
`background', `orphan' and `foreground'.
The value is always asked for and cannot be empty."
  :description "Cascade strategy"
  :class 'transient-option
  :key "-s"
  :argument "--cascade="
  :choices '("background" "orphan" "foreground")
  :always-read t
  :allow-empty nil)

(transient-define-prefix knessy-kill ()
    "Transient menu to delete the selected resource(s).

The options are passed to kubectl delete: `-s' sets the cascade
strategy, `-f' forces deletion, `-g' sets the grace period in seconds
and `-n' deletes immediately.
`--grace-period=' is incompatible with `--now' and `--force', and
`--now' with `--force'.
By default the cascade is `background' and the grace period is -1.
The `k' action runs `knessy--kill'."
    :value '("--cascade=background" "--grace-period=-1")
    :incompatible '(("--grace-period=" "--now")
                    ("--grace-period=" "--force")
                    ("--now" "--force"))
    ["Options"
     (knessy-kill:--cascade)
     ("-f" "Force"       "--force")
     ("-g" "Grace period" "--grace-period=" :reader transient-read-number)
     ("-n" "Now" "--now")]
    ["Actions"
     ("k" "Kill resources" knessy--kill)])

(defun knessy--do-every-item-in-region (f)
  "Call F on the ID of every table line in the active region.
F is a function of one argument, the ID of the entry as returned by
`tabulated-list-get-id'.
Lines without an ID are skipped.
Start from the beginning of the line where the region starts, and
stop at the end of the region.
The region is deactivated afterwards.
Point is restored, but F itself may move it.
Return value is not meaningful."
  (let ((end (region-end)))
       (save-excursion
         (goto-char (region-beginning))
         (beginning-of-line)
         (while (< (point) end)
           (when-let* ((id (tabulated-list-get-id)))
             (funcall f id))
           (forward-line 1)))
       (deactivate-mark)))

(defun knessy--ids ()
  "Return the list of IDs of the resources to work on.
Each ID is a dotted pair \(NS . \(RT . NAME)), see `knessy--id-ns'.
If the region is active, use the entries in it.
Otherwise, if some resources are marked and visible \(see
`knessy--marked-and-visible'), use them.
Otherwise use the single entry at point.
The order of the returned list is unspecified.
Deactivates the region as a side effect when it was used."
  (let* ((marked-and-visible (knessy--marked-and-visible))
         selected-ids)
    (if (use-region-p)
        (knessy--do-every-item-in-region (lambda (id)
                                           (push id selected-ids)))
      (if marked-and-visible
          (setq selected-ids marked-and-visible)
        (setq selected-ids (list (tabulated-list-get-id)))))
    selected-ids))

;; TODO (pgu, 19.05.2026): introduce additional parsing for --all-containers + timestamps
(defun knessy--logs (&optional args)
  "Show logs of the selected resources in a new log buffer.
Resources are chosen by `knessy--ids': the region, the marked
resources, or the one at point.
ARGS is the list of transient arguments of `knessy-log', or nil.
It may contain `--follow', `--all-containers', `--timestamps',
`--prefix' and `--tail=N'.
For each resource, start an asynchronous kubectl logs process with
`knessy--kubectl-log-object-async', writing into a fresh buffer.
The buffer gets the env of the current buffer, is put in
`knessy-log-mode' and is displayed with `display-buffer'.
Return value is not meaningful."
  (interactive (list (transient-args 'knessy-log)))
  (let* ((follow? (member "--follow" args))
         (all-containers? (member "--all-containers" args))
         (tail-lines (transient-arg-value "--tail=" args))
         (timestamps? (member "--timestamps" args))
         (prefix? (member "--prefix" args))

         (selected-ids (knessy--ids))

         (current-env (knessy--env))
         (log-buf (knessy--utils-fresh-kubectl-buffer "log"))
         (log-buferr (knessy--utils-fresh-kubectl-buffer "log" nil nil nil t)))

    (with-current-buffer log-buf
      (setq buffer-file-coding-system 'prefer-utf-8-unix)
      (knessy--set-env current-env)
      (dolist (id selected-ids)
          (let ((ns (knessy--id-ns id))
                (rt (knessy--id-rt id))
                (name (knessy--id-name id)))
            (knessy--kubectl-log-object-async ns rt name log-buf log-buferr follow? tail-lines all-containers? prefix? nil timestamps? t 48)))
      (knessy-log-mode)
      ;; FIXME: second time because mode change resets buffer-locals
      (knessy--set-env current-env))
    (display-buffer log-buf)))

(defun knessy--shell-read-container (prompt _initial-input _history)
  "Read a container name of the pod at point, and return it.

This is the reader of the `--container=' option of `knessy-shell'.
PROMPT is the minibuffer prompt supplied by transient.
_INITIAL-INPUT and _HISTORY are supplied by transient and unused.
With a single container there is nothing to choose, so it is returned
without asking.
Signals a `user-error' if the pod has no containers."
  (let* ((id (tabulated-list-get-id))
         (containers (knessy--pod-containers knessy--context
                                             (knessy--id-ns id)
                                             (knessy--id-name id))))
    (if (length< containers 2)
        (or (car containers) (user-error "Pod has no containers"))
      (completing-read prompt containers nil t))))

(transient-define-prefix knessy-shell ()
  "Transient menu to shell into the pod at point.

The `-c' option chooses the container, and `s' opens the shell with
`knessy-shell-into'."
  ["Options"
   ("-c" "Container" "--container=" :reader knessy--shell-read-container)]
  ["Actions"
   ("s" "Shell" knessy-shell-into)])

(defun knessy-shell-into (&optional args)
  "Shell into the pod at point, in the terminal set by `knessy-shell-terminal'.

ARGS is the list of transient arguments of `knessy-shell'.
The container comes from its `--container=' argument; without it, the
default container of the pod is used.
Signals a `user-error' if the resource at point is not a pod."
  (interactive (list (transient-args 'knessy-shell)))
  (let* ((selected-ids (knessy--ids))
         ;; TODO (pgu, 29.05.2026): in principle we could work on multiple pods, just open multiple buffers...
         (id (first selected-ids))
         (ns (knessy--id-ns id))
         (rt (knessy--id-rt id))
         (name (knessy--id-name id))
         (container (transient-arg-value "--container=" args))
         (ctx knessy--context))
    (unless (eq 'pod (ht-get knessy--resource-type-str->sym rt))
      (user-error "Can only shell into pods!"))
    (knessy-exec-pod ctx ns name container)))


(defvar knessy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'knessy-config)
    (define-key map (kbd "d") 'knessy-describe)
    ;; TODO (pgu, 15.05.2026): should go
    (define-key map (kbd "p") 'knessy-print-marked)
    (define-key map (kbd "k") 'knessy-kill)
    (define-key map (kbd "l") 'knessy-log)
    (define-key map (kbd "m") 'knessy-mark)
    (define-key map (kbd "u") 'knessy-unmark)
    (define-key map (kbd "M") 'knessy-mark-all)
    (define-key map (kbd "U") 'knessy-unmark-all)
    (define-key map (kbd "r") 'knessy-rename)
    (define-key map (kbd "b") 'knessy-switch-buffer)
    (define-key map (kbd "K") 'knessy-cleanup-buffers)
    (define-key map (kbd "g") 'knessy--display2)
    (define-key map (kbd "G") 'knessy--reset-in-progress)
    (define-key map (kbd "e") 'knessy-edit)
    (define-key map (kbd "j") 'knessy-jump)
    (define-key map (kbd "f") 'knessy-filter)
    (define-key map (kbd "F") 'knessy-port-forward)
    (define-key map (kbd "[") 'knessy-env-go-back)
    (define-key map (kbd "]") 'knessy-env-go-forward)
    (define-key map (kbd "z") 'knessy-env-history)
    (define-key map (kbd "s") 'knessy-shell)
    map)
  "Keymap for `knessy-mode'.")

;; TODO: update on every namespace switch
(defvar-local knessy--namespace-current-all? nil)

;; Source - https://stackoverflow.com/a/22460428
;; Posted by Brian Burns, modified by community.
;; Retrieved 2026-03-23, License - CC BY-SA 4.0
(defun knessy--buffer-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
BUFFER-OR-NAME is a buffer or a buffer name.
If BUFFER-OR-NAME is nil, return the current buffer's mode."
  (buffer-local-value 'major-mode
   (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun knessy--knessy-buffer? (buffer-name)
  "Return non-nil if BUFFER-NAME is the name of a Knessy buffer.
BUFFER-NAME is a string.
Knessy buffers are those whose name starts with an asterisk followed
by `knessy-base-buffer-name'; this includes auxiliary buffers."
  (s-starts-with? (s-concat "*" knessy-base-buffer-name) buffer-name))

(defun knessy--knessy-main-buffer? (buffer-name)
  "Return non-nil if BUFFER-NAME is the name of a main Knessy buffer.
BUFFER-NAME is a string.
Main buffers are those whose name starts with an asterisk, then
`knessy-base-buffer-name', then another asterisk, as generated by
`knessy--main-buffer-name'."
  (s-starts-with? (s-concat "*" knessy-base-buffer-name "*") buffer-name))

(defun knessy-switch-buffer (&optional all)
  "Switch to another Knessy buffer, chosen with `completing-read'.
Without prefix argument, ALL is nil, and only buffers in `knessy-mode'
are offered.
With prefix argument, ALL is non-nil, and every buffer matching
`knessy--knessy-buffer?' is offered, including auxiliary ones."
  (interactive "P")
  ;; TODO: can we integrate with persp-mode or perspective or projects here seamlessly?..
  (let* ((candidate-names (->> (buffer-list)
                               (mapcar #'buffer-name)
                               (-filter (lambda (s)
                                          (and
                                           (knessy--knessy-buffer? s)
                                           (if all t
                                             (eq 'knessy-mode (knessy--buffer-mode s))))))))
         (chosen-buf-name (completing-read "Choose buffer: " candidate-names)))
    (switch-to-buffer chosen-buf-name)))

(defun knessy-cleanup-buffers (&optional only-auxiliary)
  "Kill all Knessy buffers, after asking for confirmation.
With prefix argument, ONLY-AUXILIARY is non-nil, and the main Knessy
buffers are kept; only the auxiliary ones are killed.
Buffers of live port-forwards are never killed, as that would stop
the forwards.
Return value is not meaningful."
  (interactive "P")
  ;; TODO: can we integrate with persp-mode or perspective or projects here seamlessly?..
  (when (yes-or-no-p (s-concat "Really kill all " (if only-auxiliary "auxiliary " "") "Knessy buffers?"))
    ;; the buffers of the running port-forwards are not cleaned up, as that would
    ;; kill the forwards; they are stopped in the port forwarding prefix
    (if only-auxiliary
        (->> (buffer-list)
             (mapcar #'buffer-name)
             (-filter #'knessy--knessy-buffer?)
             (-remove #'knessy--knessy-main-buffer?)
             (-remove #'knessy--port-forward-buffer-live?)
             (mapcar #'kill-buffer))
      (->> (buffer-list)
           (mapcar #'buffer-name)
           (-filter #'knessy--knessy-buffer?)
           (-remove #'knessy--port-forward-buffer-live?)
           (mapcar #'kill-buffer)))))

(defun knessy-rename (name)
  "Rename the current Knessy buffer to NAME.
NAME is read from the minibuffer.
The new buffer name is built by `knessy--main-buffer-name' from
`knessy-base-buffer-name' and NAME, with an empty NAME giving the
default name.
It is made unique with `generate-new-buffer-name'."
  (interactive "MRename Knessy buffer: ")
  (let ((new-name (generate-new-buffer-name (knessy--main-buffer-name knessy-base-buffer-name name))))
    (rename-buffer new-name)))

;; TODO: maybe optimise mark repainting by changing the entry directly
(defun knessy-mark ()
  "Mark the resource at point, or all resources in the active region.
Marked IDs are stored in the buffer-local hash table `knessy--marked',
and the name column of each marked line is highlighted.
Without region, move to the next line afterwards.
With region, the region is deactivated.
Marked resources are what commands such as `knessy-log' and
`knessy-kill' work on, via `knessy--ids'."
  (interactive)
  (let* ((name-column-num (cl-position-if (lambda (x) (s-equals? "NAME" (first x)))
                                          tabulated-list-format)))
    (if (use-region-p)
        (knessy--do-every-item-in-region
         (lambda (id)
           (ht-set knessy--marked id t)
           (knessy--mark-tablist-entry id name-column-num)))
      (ht-set
       knessy--marked
       (tabulated-list-get-id)
       t)
      ;; (knessy--repaint)
      ;; TODO (pgu, 15.05.2026): maybe this is a bad idea.
      (knessy--mark-tablist-entry (tabulated-list-get-id) name-column-num)
      (forward-line))))


(defun knessy-mark-all ()
  "Mark all resources currently listed in the table.
Only the entries in `tabulated-list-entries' are marked, i.e. the
visible ones.
Mark IDs are added to `knessy--marked', and the table is redrawn with
`knessy--repaint'."
  (interactive)
  ;; by doing this based on tabulated-list-entries, we operate just
  ;; on the entries that are actually visible
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (ht-set knessy--marked id t)))
  (knessy--repaint))

(defun knessy-unmark ()
  "Unmark the resource at point, or all resources in the active region.
Remove the highlighting of the name column of each affected line.
Without region, also remove the ID from `knessy--marked' and move to
the next line afterwards.
With region, the region is deactivated, and the IDs in
`knessy--marked' are set to nil instead of being removed."
  (interactive)
  (let* ((name-column-num (cl-position-if (lambda (x) (s-equals? "NAME" (first x)))
                                          tabulated-list-format)))
    (if (use-region-p)
        (knessy--do-every-item-in-region (lambda (id)
                                           (ht-set knessy--marked id nil)
                                           (knessy--unmark-tablist-entry (tabulated-list-get-id) name-column-num)))
      (ht-remove
       knessy--marked
       (tabulated-list-get-id))
      (knessy--unmark-tablist-entry (tabulated-list-get-id) name-column-num)
      (forward-line))))

(defun knessy-unmark-all ()
  "Unmark all resources currently listed in the table.
Only the entries in `tabulated-list-entries' are unmarked, i.e. the
visible ones.
Their IDs are removed from `knessy--marked', and the table is redrawn
with `knessy--repaint'."
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (ht-remove knessy--marked id)))
  (knessy--repaint))

(defun knessy-print-marked (&optional non-visible)
  "Show the marked resources in the echo area, one message per resource.
Each message has the form RESOURCE-TYPE: NAMESPACE/NAME.
By default only marked resources that are present in the current data
\(`knessy--data') are printed.
With prefix argument, NON-VISIBLE is non-nil, and marked resources
that are not currently visible are printed as well."
  (interactive "P")
  (dolist (id (ht-keys knessy--marked))
    (when (and id
               (ht-get knessy--marked id)
               (if non-visible
                   t
                 (ht-contains? (asoc-get knessy--data :items) id)))
      (let ((ns (knessy--id-ns id))
            (rt (knessy--id-rt id))
            (name (knessy--id-name id)))
        (message "%s: %s/%s" rt ns name)))))

(defun knessy--contexts ()
  "Return the list of kubectl context names.
The list is cached in `knessy--cache' under the key (:ctx); when it is
missing or expired, `knessy--kubectl-contexts' runs kubectl
synchronously to fill it."
  (knessy--cache-get
   knessy--cache
   '(:ctx)
   (lambda ()
     (knessy--kubectl-contexts))))

;; TODO: call this when changing a namespace
(defun knessy--namespace-current-all?-update ()
  "Update `knessy--namespace-current-all?' from `knessy--namespace'.
Set the buffer-local variable to non-nil if the current namespace is
`knessy-all-namespaces-string', and to nil otherwise.
Return the new value."
  (setq knessy--namespace-current-all?
        (s-equals? knessy--namespace knessy-all-namespaces-string)))

(defun knessy--namespace-all? (namespace)
  "Return non-nil if NAMESPACE is the all-namespaces pseudo namespace.
NAMESPACE is a string, compared with `knessy-all-namespaces-string'."
  (s-equals? namespace knessy-all-namespaces-string))

(defun knessy--resource-type-all-namespaced? (resource-type)
  "Return non-nil if RESOURCE-TYPE is the all-namespaced pseudo type.
RESOURCE-TYPE is a string, compared with
`knessy-all-namespaced-resource-types-string'."
  (s-equals? resource-type knessy-all-namespaced-resource-types-string))

(defun knessy--resource-type-all-cluster? (resource-type)
  "Return non-nil if RESOURCE-TYPE is the all-cluster-scoped pseudo type.
RESOURCE-TYPE is a string, compared with
`knessy-all-cluster-resource-types-string'."
  (s-equals? resource-type knessy-all-cluster-resource-types-string))

(defun knessy--resource-type-linked? (resource-type)
  "Return non-nil if RESOURCE-TYPE is the linked resources pseudo type.
RESOURCE-TYPE is a string, compared with
`knessy-linked-resource-type-string'."
  (s-equals? resource-type knessy-linked-resource-type-string))

(defun knessy--resource-type-all-synthetic? (resource-type)
  "Return non-nil if RESOURCE-TYPE is a pseudo resource type.
RESOURCE-TYPE is a string.
It is a pseudo type if it is one of the *ALL-...* pseudo resource-types,
or the *LINKED* one.
None of them is a real resource type that kubectl
can be called with, or that label/field selectors make sense for."
  (or (knessy--resource-type-all-namespaced? resource-type)
      (knessy--resource-type-all-cluster? resource-type)
      (knessy--resource-type-linked? resource-type)))

;; TODO: all these should be customizable
;; TODO: these should be set from the actual available resources,
;;   or from history, not like this

(defcustom knessy-initial-context
  ""
  "Context at the first Knessy invocation. Leave blank to switch to current kubectl context."
  :group 'knessy
  :type 'string)

(defcustom knessy-initial-namespace
  "kube-system"
  "Namespace at the first Knessy invocation."
  :group 'knessy
  :type 'string)

(defcustom knessy-initial-resource-type
  "pods"
  "Resource type at the first Knessy invocation"
  :group 'knessy
  :type 'string)

(defvar-local knessy--namespace
  knessy-initial-namespace)

(defvar-local knessy--context
  (if (s-blank? knessy-initial-context)
      ;; FIXME: change hardcoded path here
      ;; FIXME: make it less low-level
      (let* ((buf (knessy--utils-fresh-kubectl-buffer "current-config" t t t))
             (res (if (eq 0 (knessy--shell-exec (s-concat knessy-kubectl " config current-context") buf))
                      (with-current-buffer buf
                        (s-trim (buffer-string)))
                    "default")))
        (kill-buffer buf)
        res)
    knessy-initial-context))

(comment
 (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "current-config" t t t)))))
      (knessy--shell-exec (s-concat knessy-kubectl " config current-context") buf)))


(defvar-local knessy--resource-type
    knessy-initial-resource-type)

(defun knessy--marked-and-visible ()
  "Return the list of IDs that are both marked and currently visible.
Intersect the IDs in `knessy--marked' with the items in `knessy--data'
using `knessy--utils-set-intersection'.
The order of the list is unspecified."
  (knessy--utils-set-intersection
   (asoc-get knessy--data :items)
   knessy--marked))


(defvar knessy--last-selected-namespace nil)
(defvar knessy--last-selected-context nil)
(defvar knessy--last-selected-resource-type nil)

(defun knessy--select-config-file (filename)
  "Switch to the kubeconfig file FILENAME.
FILENAME is read interactively, from `~/.kube' by default, and the
file must exist.
If FILENAME is nil, `~/.kube/config' is used.
Set `knessy-kubeconfig' to the expanded file name, and start the
asynchronous refresh of the caches with `knessy--caches-populate-async'.
Signal an error if the file does not exist."
  (interactive
   (list (read-file-name "kubeconfig: " "~/.kube" nil t)))
  (let ((filename (or filename "~/.kube/config")))
    (if (file-exists-p (expand-file-name filename))
        (progn
          (setq knessy-kubeconfig (expand-file-name filename))
          (knessy--caches-populate-async))

      (error "Kubectl config file '%s' does not exist!" filename))))

(defun knessy--print-msg ()
  "Show the message \"Hello world!\" in the echo area.
This is a debugging helper."
  (interactive)
  (message "Hello world!"))

(defun knessy--time-ms ()
  "Return the current local time as a string with millisecond precision.
The format is YYYY-MM-DD HH:MM:SS.mmm."
  (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))

(comment
 (knessy--print-time-ms))

(defcustom knessy-reset-label-selectors-on-resource-type-change nil
  "Whether to reset label filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-field-selectors-on-resource-type-change nil
  "Whether to reset field filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-label-selectors-on-namespace-change nil
  "Whether to reset label filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-field-selectors-on-namespace-change nil
  "Whether to reset field filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-label-selectors-on-context-change nil
  "Whether to reset label filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-field-selectors-on-context-change nil
  "Whether to reset field filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defun knessy--set-context (ctx)
  "Make CTX, a context name string, the context of the current buffer.
Set the buffer-local `knessy--context' and the global
`knessy--last-selected-context'.
Reset the buffer-local label and field selectors if
`knessy-reset-label-selectors-on-context-change' or
`knessy-reset-field-selectors-on-context-change' is non-nil.
This does not redisplay anything."
  (setq knessy--context ctx)
  (setq knessy--last-selected-context knessy--context)
  (when knessy-reset-label-selectors-on-context-change
    (setq knessy--label-selectors '()))
  (when knessy-reset-field-selectors-on-context-change
    (setq knessy--field-selectors '())))

;; TODO: *all*?.. contexts
(defun knessy--select-context ()
  "Ask for a context with `completing-read' and make it the current one.
Candidates come from `knessy--contexts'.
The chosen context is applied with `knessy--set-context'."
  (interactive)
  (when-let* ((new-ctx (completing-read "Context: "
                                        (knessy--contexts))))
    (knessy--set-context new-ctx)))

(defun knessy--set-namespace (ns)
  "Make NS, a namespace name string, the namespace of the current buffer.
Use `knessy-all-namespaces-string' for all namespaces.
Set the buffer-local `knessy--namespace', update
`knessy--namespace-current-all?' and set the global
`knessy--last-selected-namespace'.
Reset the buffer-local label and field selectors if
`knessy-reset-label-selectors-on-namespace-change' or
`knessy-reset-field-selectors-on-namespace-change' is non-nil.
This does not redisplay anything."
  (setq knessy--namespace
        ns)
  (knessy--namespace-current-all?-update)
  (setq knessy--last-selected-namespace knessy--namespace)
  (when knessy-reset-label-selectors-on-namespace-change
    (setq knessy--label-selectors '()))
  (when knessy-reset-field-selectors-on-namespace-change
    (setq knessy--field-selectors '())))

(defun knessy--namespace-completion-table (namespaces)
  "Completion table for NAMESPACES with category `knessy-namespace'."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . knessy-namespace))
      (complete-with-action action namespaces string pred))))

(defvar knessy--namespace-select-buffer nil
  "Knessy buffer from which the namespace was last being selected.
Embark actions may run with another buffer current (e.g. the describe buffer
left by a previous action), whose buffer-local context is not the one chosen.")

(defun knessy--select-namespace ()
  "Ask for a namespace with `completing-read' and make it the current one.
Candidates come from `knessy--namespaces', and have the completion
category `knessy-namespace', so Embark actions are available on them.
Remember the current buffer in `knessy--namespace-select-buffer'.
The chosen namespace is applied with `knessy--set-namespace'."
  (interactive)
  (setq knessy--namespace-select-buffer (current-buffer))
  (let ((new-ns (completing-read "Namespace: "
                                 (knessy--namespace-completion-table
                                  (knessy--namespaces)))))
    (knessy--set-namespace new-ns)))

(defun knessy--embark-namespace-describe (ns)
  "Describe namespace NS, a namespace name string.
This is an Embark action on `knessy-namespace' candidates.
It runs `knessy--describe-object' in the Knessy buffer remembered in
`knessy--namespace-select-buffer', if it is still live, so the right
context is used.
Signal a `user-error' if NS is `knessy-all-namespaces-string'."
  (interactive "sNamespace: ")
  (when (equal ns knessy-all-namespaces-string)
    (user-error "Not a single namespace"))
  (with-current-buffer (if (buffer-live-p knessy--namespace-select-buffer)
                           knessy--namespace-select-buffer
                         (current-buffer))
    (knessy--describe-object nil "namespaces" ns)))

(defun knessy--embark-namespace-edit (ns)
  "Edit namespace NS, a namespace name string, as YAML.
This is an Embark action on `knessy-namespace' candidates.
It runs `knessy--edit-object' in the Knessy buffer remembered in
`knessy--namespace-select-buffer', if it is still live, so the right
context is used.
Signal a `user-error' if NS is `knessy-all-namespaces-string'."
  (interactive "sNamespace: ")
  (when (equal ns knessy-all-namespaces-string)
    (user-error "Not a single namespace"))
  (with-current-buffer (if (buffer-live-p knessy--namespace-select-buffer)
                           knessy--namespace-select-buffer
                         (current-buffer))
    (knessy--edit-object nil "namespaces" ns)))

(with-eval-after-load 'embark
  (defvar-keymap knessy-embark-namespace-map
    :doc "Embark actions for Knessy namespace candidates."
    :parent embark-general-map
    "d" #'knessy--embark-namespace-describe
    "e" #'knessy--embark-namespace-edit)
  (add-to-list 'embark-keymap-alist '(knessy-namespace . knessy-embark-namespace-map)))

(defun knessy--set-resource-type (resource-type)
  "Make RESOURCE-TYPE, a resource type string, the type of the current buffer.
Set the buffer-local `knessy--resource-type' and the global
`knessy--last-selected-resource-type'.
Clear `knessy--linked-root' unless RESOURCE-TYPE is the linked
pseudo type.
Set `knessy--view' to the last view selected for RESOURCE-TYPE, or
else to its default view.
Clear `knessy--table-remember-pos'.
Reset the buffer-local label and field selectors if
`knessy-reset-label-selectors-on-resource-type-change' or
`knessy-reset-field-selectors-on-resource-type-change' is non-nil.
This does not redisplay anything."
  (setq knessy--resource-type resource-type)
  ;; the root only makes sense for the linked view, don't carry it elsewhere
  (unless (knessy--resource-type-linked? resource-type)
    (setq knessy--linked-root nil))
  (setq knessy--view
        (or (ht-get knessy--last-selected-view knessy--resource-type nil)
            (asoc-get knessy-default-view-alist knessy--resource-type knessy-default-view-string)))
  (setq knessy--table-remember-pos nil)
  (setq knessy--last-selected-resource-type knessy--resource-type)
  (when knessy-reset-label-selectors-on-resource-type-change
    (setq knessy--label-selectors '()))
  (when knessy-reset-field-selectors-on-resource-type-change
    (setq knessy--field-selectors '())))

;; TODO: *all*!
;; TODO: reset labels and fields on resource type, ns, ctx change? make it configurable?
;; TODO: maybe also reset the regex filter
(defvar knessy--resource-type-annotation-context nil
  "Context whose api-resources `knessy--resource-type-annotation' looks at.
Bound while selecting a resource type, as the minibuffer's own buffer-local
`knessy--context' is not the one the user is working with.")

(defun knessy--resource-type-annotation-fields (resource-type)
  "Return (SHORT KIND SCOPE) strings describing RESOURCE-TYPE, or nil.
SHORT is empty if there's no short name worth showing or
`knessy-use-short-resource-type-names' is nil."
  (when-let* ((resource (knessy--api-resource resource-type
                                              knessy--resource-type-annotation-context)))
    (let ((short (knessy--resource-type-short resource-type
                                              knessy--resource-type-annotation-context)))
      (list (if (equal short resource-type) "" short)
            (or (asoc-get resource :kind) "")
            (if (asoc-get resource :namespaced) "namespaced" "cluster")))))

(defun knessy--resource-type-completion-table (resource-types)
  "Completion table for RESOURCE-TYPES with category `knessy-resource-type'."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . knessy-resource-type)
                   (annotation-function
                    . (lambda (cand)
                        (when-let* ((fields (knessy--resource-type-annotation-fields cand)))
                          (concat "  " (propertize (s-join "  " (-remove #'s-blank? fields))
                                                   'face 'completions-annotations))))))
      (complete-with-action action resource-types string pred))))

(with-eval-after-load 'marginalia
  (defun knessy--marginalia-resource-type (cand)
    "Marginalia annotator for `knessy-resource-type' candidates.
Fields have fixed widths so they line up between candidates."
    (when-let* ((fields (knessy--resource-type-annotation-fields cand)))
      (marginalia--fields
       ((nth 0 fields) :width 15 :truncate 15 :face 'marginalia-key)
       ((nth 1 fields) :width 26 :truncate 26 :face 'marginalia-documentation)
       ((nth 2 fields) :width 10 :face 'marginalia-type))))
  (add-to-list 'marginalia-annotators
               '(knessy-resource-type knessy--marginalia-resource-type builtin none)))

(defun knessy--select-resource-type ()
  "Ask for a resource type with `completing-read' and make it current.
Candidates come from `knessy--resource-types', and have the completion
category `knessy-resource-type', annotated with short name, kind and
scope.
The chosen type is applied with `knessy--set-resource-type'."
  (interactive)
  (let ((knessy--resource-type-annotation-context knessy--context))
    (when-let* ((new-rt (completing-read "Kind: "
                                         (knessy--resource-type-completion-table
                                          (knessy--resource-types)))))
      (knessy--set-resource-type new-rt))))

(defvar knessy--resource-type-children
  (ht ('deployment 'rs)
      ('argo-rollout 'rs)
      ('statefulset 'pod)
      ('daemonset 'pod)
      ('rs 'pod)
      ;; should be in some special list
      ('node 'pod)))

;; TODO: it would be great to just pass some jsonpath like .spec.selector.matchLabels=labels or metadata.name=spec.nodeName for jumping to children and back

(defvar knessy--resource-type-str->sym
  (ht ("deployments.apps" 'deployment)
      ("Deployment" 'deployment)
      ("statefulsets.apps" 'statefulset)
      ("StatefulSet" 'statefulset)
      ("daemonsets.apps" 'daemonset)
      ("DaemonSet" 'daemonset)
      ("replicasets.apps" 'rs)
      ("ReplicaSet" 'rs)
      ("pods" 'pod)
      ("Pod" 'pod)
      ("nodes" 'node)
      ("Node" 'node)
      ("rollouts.argoproj.io" 'argo-rollout)
      ("Rollout" 'argo-rollout)))

(defvar knessy--resource-type-sym->str-list
  (let ((res (ht)))
    (dolist (item (ht-items knessy--resource-type-str->sym) res)
      (let ((str (car item))
            (sym (cadr item)))
        (unless (ht-contains? res sym)
          (ht-set res sym '()))
        (ht-update-with! res sym (lambda (l) (cons str l)) '())))))

(defun knessy--resource-type-child (owner-resource-type)
  "Return the child resource type symbol of OWNER-RESOURCE-TYPE, or nil.
OWNER-RESOURCE-TYPE is a resource type string.
The result is looked up in `knessy--resource-type-children'.
This function appears to be unused and not to work: it calls
`knessy--res', which is not defined, and calls the hash table
`knessy--resource-type-str->sym' as a function."
  (knessy--res)
  (ht-get knessy--resource-type-children
          (knessy--resource-type-str->sym owner-resource-type)))

(defvar-local
    knessy--view
    knessy-default-view-string)

(defvar knessy--last-selected-view (ht))

(defvar-local
    knessy--table-remember-pos
    t)

(defun knessy--set-view (resource-type view)
  "Make VIEW the view of the current buffer, and remember it.
VIEW is a view name string, and RESOURCE-TYPE the resource type string
it was chosen for.
Set the buffer-local `knessy--view', and record VIEW for RESOURCE-TYPE
in `knessy--last-selected-view'.
If VIEW differs from the old view, clear `knessy--table-remember-pos'.
This does not redisplay anything."
  (let ((old-view knessy--view))
    (setq knessy--view view)
    (ht-set knessy--last-selected-view resource-type view)
    (unless (eq old-view view)
      (setq knessy--table-remember-pos nil))))

(defun knessy--select-view ()
  "Ask for a view with `completing-read' and make it the current one.
Candidates are the views defined for `knessy--resource-type' in
`knessy--views-by-resource-type', or only the default view if there
are none.
The choice must be one of the candidates, and is applied with
`knessy--set-view'."
  (interactive)
  (when-let* ((new-view (completing-read
                         "View: "
                         (ht-get knessy--views-by-resource-type knessy--resource-type (list knessy-default-view-string))
                         nil
                         t)))
    (knessy--set-view knessy--resource-type new-view)))

(transient-define-prefix
  knessy-config ()
  "Transient menu to configure the current Knessy buffer.

The actions select the resource type `t', namespace `n', context `c',
kubeconfig file `f' and view `v'."
  ["Configure"
     ("t" "resource-type" knessy--select-resource-type)
     ("n" "namespace" knessy--select-namespace)
     ("c" "context" knessy--select-context)
     ("f" "config-file" knessy--select-config-file)
     ("v" "view" knessy--select-view)])

(defcustom knessy-temp-file-dir (file-name-concat temporary-file-directory "knessy")
  "Directory to put temporary Knessy files")

(defun knessy-edit (&optional json?)
  "Edit the resource at point as YAML in a temporary file buffer.
With prefix argument, JSON? is non-nil, and the resource is edited as
JSON instead.
Only the resource at point is used, not the marked ones.
See `knessy--edit-object' for the details."
  (interactive "P")
  (let ((selected-id (tabulated-list-get-id)))
    (knessy--edit-object (knessy--id-ns selected-id)
                         (knessy--id-rt selected-id)
                         (knessy--id-name selected-id)
                         json?)))

(defun knessy--edit-object (ns rt name &optional json?)
  "Fetch an object into a temporary file and display it for editing.
NS is the namespace string, or nil for cluster-scoped objects.
RT is the resource type string and NAME the object name.
If JSON? is non-nil, use JSON and a `.json' file; otherwise YAML and a
`.yaml' file.
The file is created in `knessy-temp-file-dir', and its content is
fetched synchronously with `knessy--kubectl-get-object-sync'.
The file buffer gets the env of the current buffer, is saved, put in
`knessy-json-mode' or `knessy-yaml-mode', and displayed with
`display-buffer'.
Return value is not meaningful."
  (let* ((current-env (knessy--env))
         filename
         file-buf)
    (let* ((temporary-file-directory knessy-temp-file-dir))
      (setq filename (make-temp-file
                      (s-concat "knessy_"
                                knessy--context "_"
                                rt "_"
                                (if ns (s-concat ns "_") "")
                                name "_")
                      nil
                      (if json? ".json" ".yaml")))
      (setq file-buf (find-file-literally filename)))
    (with-current-buffer file-buf
      (setq buffer-file-coding-system 'prefer-utf-8-unix)
      (knessy--set-env current-env)
      (knessy--kubectl-get-object-sync ns rt name file-buf (if json? :json :yaml))
      (goto-char (point-min))
      (save-buffer)
      (if json?
          (knessy-json-mode)
        (knessy-yaml-mode))
      ;; FIXME: second time because mode change resets buffer-locals
      (knessy--set-env current-env))
    (display-buffer file-buf)))

(defun knessy-describe ()
  "Show the kubectl describe output for the resource at point.
Only the resource at point is used, not the marked ones.
See `knessy--describe-object' for the details."
  (interactive "")
  (let ((selected-id (tabulated-list-get-id)))
    (knessy--describe-object (knessy--id-ns selected-id)
                             (knessy--id-rt selected-id)
                             (knessy--id-name selected-id))))

(defun knessy--describe-object (ns rt name)
  "Describe an object in a new buffer and display it.
NS is the namespace string, or nil for cluster-scoped objects.
RT is the resource type string and NAME the object name.
Run kubectl describe synchronously with
`knessy--kubectl-describe-object-sync', put the resulting buffer in
`knessy-describe-mode', and show it with `display-buffer'.
Return value is not meaningful."
  (let ((obj-buf (knessy--kubectl-describe-object-sync ns rt name)))
    (with-current-buffer obj-buf
      (knessy-describe-mode)
      (goto-char (point-min)))
    (display-buffer obj-buf)))

;; TODO (pgu, 19.05.2026): maybe do prefix via transient so no-prompt is obvious
;; FIXME (pgu, 19.05.2026): doesn't work with *ALL* namespaces
(defun knessy--kill (&optional args prefix)
  "Delete the selected resources with kubectl delete.
Resources are chosen by `knessy--ids': the region, the marked
resources, or the one at point.
ARGS is the list of transient arguments of `knessy-kill', or nil.
It may contain `--force', `--now' and `--grace-period=N'.
PREFIX is non-nil with a prefix argument, and then no confirmation
is asked; otherwise ask with `yes-or-no-p', and signal a `user-error'
if the answer is no.
For each resource, start an asynchronous kubectl delete process with
`knessy--kubectl-delete-object-async', writing into fresh buffers.
Return value is not meaningful."
  (interactive (list (transient-args 'knessy-kill) current-prefix-arg))
  (let* ((force? (member "--force" args))
         (grace-period (transient-arg-value "--grace-period=" args))
         (now? (member "--now" args))
         (selected-ids (knessy--ids))
         (kill-buf (knessy--utils-fresh-kubectl-buffer "kill"))
         (kill-buferr (knessy--utils-fresh-kubectl-buffer "kill" nil nil nil t)))
    ;; TODO (pgu, 18.05.2026): here, detect number of different resource types and bulk-delete them
    (if (or prefix (yes-or-no-p (s-concat "Killing " (s-join ", " (mapcar #'knessy--id-name selected-ids)) ", proceed? ")))
        (dolist (id selected-ids)
          (let ((ns (knessy--id-ns id))
                (rt (knessy--id-rt id))
                (name (knessy--id-name id)))
            (knessy--kubectl-delete-object-async ns rt name kill-buf kill-buferr force? grace-period now?)))
      (user-error "Aborted"))))


;; TODO (pgu, 19.05.2026): extract to knessy-transient.el
(defun transient-read-number (prompt initial-input history)
  "Read an integer in the minibuffer and return it as a string.
PROMPT is the minibuffer prompt.
INITIAL-INPUT is the initial minibuffer contents, or nil.
HISTORY is the minibuffer history variable to use, or nil.
Despite the name, zero and negative integers are accepted too, and so
is the empty string.
This is the reader function used by Knessy transient arguments.
It delegates to `transient--read-number-any'."
  (transient--read-number-any prompt initial-input history))

(defun transient--read-number-any (prompt initial-input history)
  "Read an integer in the minibuffer and return it as a string.
PROMPT is the minibuffer prompt.
INITIAL-INPUT is the initial minibuffer contents, or nil.
HISTORY is the minibuffer history variable to use, or nil.
Accept the empty string, 0, or an optionally negative integer with no
leading zeros.
Otherwise show a message for a second and prompt again, until the input
is valid."
  (save-match-data
    (cl-block nil
      (while t
        (let ((str (read-from-minibuffer prompt initial-input nil nil history)))
          (when (or (string-equal str "")
                    (string-match-p "\\`\\(0\\|-?[1-9][0-9]*\\)\\'"
                                    str))
            (cl-return str)))
        (message "Please enter an integer number.")
        (sit-for 1)))))

(comment
 (string-match-p "\\`\\(0\\|-?[1-9][0-9]*\\)\\'"
                                    "-123"))

(comment
 (append (vector 1 2 3) nil)
 (asoc)
 (asoc-put! '((apple . 1) (banana . 2)) 'a 'b)
 (let ((my-alist '((apple . 1) (banana . 2))))
  ;; Update existing key
  (asoc-put! my-alist 'apple 10)
  ;; Add new key
  (asoc-put! my-alist 'orange 3)
  my-alist)
 (asoc-zip '("spec.nodeName") '("blah")))

(defun knessy--jump-namespace ()
  "Switch the buffer to the namespace of the resource at point.
Set the buffer namespace to that of the resource on the current line.
Clear both field and label selectors, then redisplay the buffer.
Signal a `user-error' if the resource is not namespaced."
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id)))
    (when (null ns)
      (user-error "This command should be used on a namespaced object."))
    (knessy--set-namespace ns)
    (setq knessy--field-selectors nil)
    (setq knessy--label-selectors nil)
    (knessy--display2)))

(defun knessy--jump-node ()
  "Show the node that the pod at point is running on.
Fetch the pod with kubectl synchronously and read its spec.nodeName.
Switch the buffer to the \"nodes\" resource type, with a field selector
on metadata.name set to that node and no label selectors, then redisplay.
Signal a `user-error' if the resource at point is not a pod.
Do nothing if the pod has no node assigned yet."
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name))
         (obj-sym (ht-get knessy--resource-type-str->sym rt)))
    (when (not (eq obj-sym 'pod))
      (user-error "This command should be used on a pod."))
    (when-let* ((nodename (ht-get* obj "spec" "nodeName")))
      (knessy--set-resource-type "nodes")
      (setq knessy--field-selectors (asoc-zip '("metadata.name") (list nodename)))
      (setq knessy--label-selectors nil)
      (knessy--display2))))

(defun knessy--jump-node-pods ()
  "Show all the pods running on the node of the resource at point.
The resource at point must be a pod or a node.
For a pod, use its spec.nodeName; for a node, use its own name.
Fetch the object with kubectl synchronously.
Switch the buffer to the \"pods\" resource type in all namespaces, with a
field selector on spec.nodeName and no label selectors, then redisplay.
Signal a `user-error' if the resource at point is neither a pod nor a node.
Do nothing if no node name could be determined."
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name))
         (obj-sym (ht-get knessy--resource-type-str->sym rt)))
    (when (not (or (eq obj-sym 'pod)
                   (eq obj-sym 'node)))
      (user-error "This command should be used on a pod or a node."))
    (when-let* ((nodename
                 (cond ((eq obj-sym 'pod)
                        (ht-get* obj "spec" "nodeName"))
                       ((eq obj-sym 'node)
                        (ht-get* obj "metadata" "name")))))
      (knessy--set-resource-type "pods")
      (knessy--set-namespace knessy-all-namespaces-string)
      (setq knessy--field-selectors (asoc-zip '("spec.nodeName") (list nodename)))
      (setq knessy--label-selectors nil)
      (knessy--display2))))

(defun knessy--jump-children ()
  "Show the child resources of the resource at point.
The child resource type is found with `knessy--child-resource-type'.
Fetch the object with kubectl synchronously.
For a node, show the children type filtered by a spec.nodeName field
selector equal to the node name.
For any other resource, filter the children type by the label selector
made of the spec.selector.matchLabels of the object.
In both cases clear the other kind of selector and redisplay the buffer.
Signal an `error' if the object has no spec, no selector or no
matchLabels."
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name))
         (owner-sym (ht-get knessy--resource-type-str->sym rt))
         (children-resource-type (knessy--child-resource-type rt)))
    (cond ((eq owner-sym 'node)
           ;; TODO: implement this!
           (progn
             ;; (setq knessy--resource-type children-resource-type)
             (knessy--set-resource-type children-resource-type)
             (setq knessy--field-selectors (asoc-zip '("spec.nodeName") (list name)))
             (setq knessy--label-selectors nil)
             (knessy--display2)))

          (t
           (if-let* ((spec (ht-get obj "spec")))
            (if-let* ((selector (ht-get spec "selector")))
                (if-let* ((match-labels (ht-get selector "matchLabels")))
                    (progn
                      (knessy--set-resource-type children-resource-type)
                      (setq knessy--label-selectors (ht->alist match-labels))
                      (setq knessy--field-selectors nil)
                      (knessy--display2))

                  (error "Object selector has no matchLabels"))
              (error "Object spec has no selector"))
            (error "Object has no spec"))))))

(defun knessy--jump-linked ()
  "Show all the resources linked (by ownership or references) to the selected one.
Switch the buffer to the linked resource type and store the resource at
point as `knessy--linked-root'.
Set the namespace to that of the resource, if it has one.
Clear field and label selectors, then redisplay.
Signal a `user-error' if there is no resource on the current line."
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id)))
    (unless selected-id
      (user-error "No resource on this line"))
    (knessy--set-resource-type knessy-linked-resource-type-string)
    (when ns
      (knessy--set-namespace ns))
    (setq knessy--linked-root (list ns rt name))
    (setq knessy--field-selectors nil)
    (setq knessy--label-selectors nil)
    (knessy--display2)))

(defun knessy--kind->resource-type (kind)
  "Return the resource type string for the Kubernetes KIND.
KIND is a kind name such as \"ReplicaSet\", as found in ownerReferences.
Among the resource type names known for the kind, return the first one
that is present in `knessy--resource-types-set'.
Return nil if there is none."
  (let*((sym (ht-get knessy--resource-type-str->sym kind))
         (types (ht-get knessy--resource-type-sym->str-list sym)))
    (cl-loop for type in types
             when (ht-contains? (knessy--resource-types-set) type)
             return type)))

(defun knessy--child-resource-type (owner-resource-type)
  "Return the resource type string of the children of OWNER-RESOURCE-TYPE.
OWNER-RESOURCE-TYPE is a resource type string such as \"deployments\".
Look up the child type in `knessy--resource-type-children', then return
the first of its names present in `knessy--resource-types-set'.
Return nil if there is none."
  (let*((sym (ht-get knessy--resource-type-str->sym owner-resource-type))
         (child-sym (ht-get knessy--resource-type-children sym))
         (types (ht-get knessy--resource-type-sym->str-list child-sym)))
    (cl-loop for type in types
             when (ht-contains? (knessy--resource-types-set) type)
             return type)))

(comment
 (ht-contains? (knessy--resource-types-set) "replicasets.apps"))

;; TODO: handle the errors (no owner, no metadata etc)
(defun knessy--jump-owner ()
  "Show the owner of the resource at point.
Fetch the object with kubectl synchronously and take the first entry of
its metadata.ownerReferences.
Switch the buffer to the resource type of the owner kind, with a field
selector on metadata.name set to the owner name and no label selectors.
Redisplay by calling `knessy--display2' interactively, so a prefix
argument makes the redisplay synchronous.
Signal an `error' if the object has no metadata or no ownerReferences, or
if they are empty."
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name)))
    (if-let* ((metadata (ht-get obj "metadata")))
        (if-let* ((owner-refs (ht-get metadata "ownerReferences")))
            (if (> (length owner-refs) 0)
                (let* ((owner-ref (aref owner-refs 0))
                       (owner-name (ht-get owner-ref "name"))
                       (owner-kind (ht-get owner-ref "kind"))
                       (owner-resource-type (knessy--kind->resource-type owner-kind)))
                  (message "owner kind %s" owner-kind)
                  (message "owner resource type %s" owner-resource-type)
                  (knessy--set-resource-type owner-resource-type)
                  (setq knessy--field-selectors `(("metadata.name" . ,owner-name)))
                  (setq knessy--label-selectors nil)
                  (call-interactively #'knessy--display2))
              (error "Object ownerReferences are empty"))
          (error "Object metadata has no ownerReferences"))
      (error "Object has no metadata"))))

(defvar-local knessy--field-selectors '())

(defvar knessy--field-selectors-fields-ht
  (ht ('pod '("spec.nodeName"
              "spec.restartPolicy"
              "spec.schedulerName"
              "spec.serviceAccountName"
              "spec.hostNetwork"
              "status.phase"
              "status.podIP"
              "status.podIPs"
              "status.nominatedNodeName"))
      ('event '("involvedObject.kind"
                "involvedObject.namespace"
                "involvedObject.name"
                "involvedObject.uid"
                "involvedObject.apiVersion"
                "involvedObject.resourceVersion"
                "involvedObject.fieldPath"
                "reason"
                "reportingComponent"
                "source"
                "type"))
      ('secret '("type"))
      ('namespace '("status.phase"))
      ('rs '("status.replicas")) ;; FIXME: doesn't work?
      ('rscontroller '("status.replicas"))
      ('job '("status.successful"))
      ('node '("spec.unschedulable"))
      ('csr '("spec.signerName"))))

(defvar knessy--field-selectors-fields-common
  '("metadata.name" "metadata.namespace"))

(defun knessy--field-selectors-fields (resource-type)
  "Return the list of field names usable as field selectors.
RESOURCE-TYPE is a resource type string, such as \"pods\".
The result is `knessy--field-selectors-fields-common' followed by the
fields registered for that type in `knessy--field-selectors-fields-ht',
if any."
  (append
   knessy--field-selectors-fields-common
   (ht-get knessy--field-selectors-fields-ht
           (ht-get knessy--resource-type-str->sym resource-type))))

(defcustom knessy-field-selector-finish-choice "*FINISH*"
  "String to use to finish field selectors choosing process"
  :group 'knessy
  :type 'string)

(comment
 (read-string)
 (append '(1) '(2)))

;; TODO: finish this, so we can jump to children
(defun knessy--filter-field-selector ()
  "Interactively choose field selectors for the current buffer.
Repeatedly prompt for a field name of the current resource type, then
for its value, collecting field=value pairs.
Choosing `knessy-field-selector-finish-choice' ends the loop.
The collected pairs replace `knessy--field-selectors' in the current
buffer.
The buffer is not redisplayed.
Signal a `user-error' if the resource type does not support selectors."
  (interactive)
  (when (knessy--resource-type-all-synthetic? knessy--resource-type)
    (user-error "Field selectors are not supported for the %s resource-type" knessy--resource-type))
  (let ((current-field nil)
        (current-value nil)
        (new-filters '())
        ;; if the namespace is ALL or the resource-type is global
        (fields (knessy--field-selectors-fields knessy--resource-type)))
    (while (not (s-equals? current-field knessy-label-selector-finish-choice))
      (setq current-field
            (completing-read
             (s-concat "(" (if new-filters
                               (knessy--utils-alist->str-= new-filters)
                             (knessy--utils-alist->str-= knessy--field-selectors))
                       ") Select field: ")
             (cons knessy-field-selector-finish-choice
                   fields)))
      (unless (s-equals? current-field knessy-field-selector-finish-choice)
        ;; TODO: I'm pretty sure we can do completing-read here
        (setq current-value
              (read-string
               "Enter value: "))
        (push (cons current-field current-value) new-filters)))
    (setq knessy--field-selectors new-filters)))

(comment
 (catch)
 (ht-get nil 'key))

(transient-define-prefix
  knessy-jump ()
  "Show a menu of jumps from the resource at point to related resources.
The jumps go to children, linked resources, the owner, the node, the pods
of a node, and the namespace."
  ["Jump to"
   ("c" "children" knessy--jump-children)
   ("l" "linked" knessy--jump-linked)
   ("o" "owner" knessy--jump-owner)
   ("n" "node" knessy--jump-node)
   ("p" "node pods" knessy--jump-node-pods)
   ("N" "namespace" knessy--jump-namespace)])

(defcustom knessy-label-selector-finish-choice
  "*FINISH*"
  "Item to indicate finishing choosing label selectors."
  :type 'string
  :group 'knessy)

(defvar knessy-label-cache-ttl 120
  "TTL for labels to live")

(defvar-local knessy--label-selectors '())

(defun knessy--resource-type-namespaced? (resource-type)
  "Return non-nil if RESOURCE-TYPE is namespaced, nil if it is global.
RESOURCE-TYPE is a resource type string, such as \"pods\".
Consult `knessy--resource-types-namespaced'."
  (ht-contains? (knessy--resource-types-namespaced) resource-type))

(defun knessy--resource-type-global? (resource-type)
  "Return non-nil if RESOURCE-TYPE is cluster-scoped (not namespaced).
RESOURCE-TYPE is a resource type string, such as \"nodes\"."
  (not (knessy--resource-type-namespaced? resource-type)))

(defun knessy--filter-label ()
  "Interactively choose label selectors for the current buffer.
Repeatedly prompt for a label key, then for one of its known values,
collecting key=value pairs.
Choosing `knessy-label-selector-finish-choice' ends the loop.
Candidates come from the labels cache in `knessy--cache', populated with
`knessy--kubectl-labels' if needed.
The cache is keyed by context and resource type, plus the namespace
unless the namespace is all or the resource type is global.
The collected pairs replace `knessy--label-selectors' in the current
buffer.
The buffer is not redisplayed.
Signal a `user-error' if the resource type does not support selectors."
  (interactive)
  (when (knessy--resource-type-all-synthetic? knessy--resource-type)
    (user-error "Label selectors are not supported for the %s resource-type" knessy--resource-type))
  (let ((current-label nil)
        (current-value nil)
        (new-selectors '())
        ;; if the namespace is ALL or the resource-type is global
        (lookup-keys (if (or (knessy--namespace-all? knessy--namespace) (knessy--resource-type-global? knessy--resource-type))
                         (list :labels knessy--context knessy--resource-type)
                       (list :labels knessy--context knessy--namespace knessy--resource-type))))
    (while (not (s-equals? current-label knessy-label-selector-finish-choice))
      (setq current-label
            (completing-read
             (s-concat "(" (if new-selectors
                               (knessy--utils-alist->str-= new-selectors)
                             (knessy--utils-alist->str-= knessy--label-selectors))
                       ") Select label: ")
             (cons knessy-label-selector-finish-choice
                   (ht-keys
                    (knessy--cache-get knessy--cache lookup-keys #'knessy--kubectl-labels)))))
      (unless (s-equals? current-label knessy-label-selector-finish-choice)
        (setq current-value
              (completing-read
               "Select value: "
               (ht-keys
                (ht-get
                 (knessy--cache-get knessy--cache lookup-keys #'knessy--kubectl-labels)
                 current-label))))
        (push (cons current-label current-value) new-selectors)))

    (setq knessy--label-selectors new-selectors)))

(comment
 (let ((knessy--context "minikube")
       (knessy--namespace "kube-system")
       (knessy--resource-type "pods"))
   (knessy--filter-label)))

(defvar-local knessy--regex "")
(defvar-local knessy--regex-hide nil)

(defun knessy--set-regex ()
  "Prompt for a regex and set it as the filter of the current buffer.
Store it in `knessy--regex', with the old value as the initial input, and
repaint the table.
No kubectl call is made.
The regex is not validated."
  (interactive)
  (let ((regex (read-string "Regex: " knessy--regex)))
    ;; TODO: verify if regex is valid
    (setq knessy--regex regex))
  ;; TODO (pgu, 19.05.2026): maybe remove repaint?
  (knessy--repaint))


(defun knessy--toggle-regex-hide ()
  "Toggle `knessy--regex-hide' in the current buffer and repaint the table.
Echo whether the regex filter hide is now on or off."
  (interactive)
  (setq knessy--regex-hide (not knessy--regex-hide))
  (knessy--log 4 (format "Regex Hide is now %s" knessy--regex-hide))
  (message "Regex filter hide is %s" (if knessy--regex-hide "on" "off"))
  ;; TODO (pgu, 19.05.2026): maybe remove repaint?
  (knessy--repaint))


(defun knessy--clear-filters ()
  "Clear the field and label selectors of the current buffer.
The buffer is not redisplayed, and the regex filter is left untouched."
  (interactive)
  (setq knessy--field-selectors '())
  (setq knessy--label-selectors '()))

(transient-define-prefix
  knessy-filter ()
  "Show a menu of filters for the current Knessy buffer.
The entries clear selectors, choose label or field selectors, set the
regex, and toggle the regex hide mode."
  ["Filter"
   ("c" "clear" knessy--clear-filters)
   ("l" "label" knessy--filter-label)
   ("f" "field" knessy--filter-field-selector)
   ("r" "regex" knessy--set-regex)
   ("h" "regex-hide" knessy--toggle-regex-hide)])

(defvar-local knessy--data nil
  "Data that was received with the latest query.")

(defvar knessy--marked (ht)
  "Set of resource IDs that are marked (selected).")

;; TODO: this will go as it's not modular enough
(defun knessy--make-display-callback (data-buf display-buf)
  "Return a callback that parses kubectl output and repaints the table.
DATA-BUF is the buffer holding the kubectl table output.
DISPLAY-BUF is the Knessy buffer to repaint.
The returned function takes no arguments.
When called it stores the parsed output in `knessy--data', then calls
`knessy--repaint' on DISPLAY-BUF."
  (lambda ()
    (setq knessy--data (knessy--parse-table-kubectl-output data-buf))
    (knessy--repaint display-buf)))


(defun knessy--repaint (&optional buf)
  "Repaint (or rather re-render the table).
BUF is buffer with the table, must be in Knessy mode.
If omitted, use the current one (for synchronous calls).
Build the table from the headers and items in `knessy--data' of that
buffer, using `knessy--make-tablist'.
If there are no items, show a single \"No resources found.\" row and
forget the remembered cursor position."

  (with-current-buffer (if buf buf (current-buffer))
    (let ((columns (-> knessy--data (asoc-get :headers) (asoc-get :static)))
          (rename (-> knessy--data (asoc-get :headers) (asoc-get :rename)))
          (widths (-> knessy--data (asoc-get :headers) (asoc-get :widths)))
          (items (asoc-get knessy--data :items)))
      (if (and (ht? items) (> (ht-size items) 0))
          (knessy--make-tablist columns rename items widths)
        (let ((item (ht ("NAME" "No resources found."))))
          (setq knessy--table-remember-pos nil)
          (knessy--make-tablist '("NAME")
                                (ht)
                                (ht (nil item))
                                (ht ("NAME" 8)))))))
  (knessy--log 1 "Done!")
  (knessy--log 4 (format "Now: %s" (current-time-string))))

(comment
 (let ((abc 'def))
   (ht (nil abc))))

;; generic display function arch:
;; await on promises from N kubectl queries
;; after that, consolidate the data:
;; - match by IDs
;; - merge the columns
;; - run max on column widths
;; - if ID is present in one map but not in another, drop the item altogether (race with some cluster events)
;; - put the whole thing in a buffer-local variable for later manipulation

;; TODO: build a simple two-step view, like ordinary get + custom column, use it to test the above
;; TODO: make the display function use aio, to wait on multiple things in parallel


(aio-defun knessy--perform-calls-async (calls)
  "Start a kubectl process for each of CALLS and return the list of promises.
CALLS is a list of call alists, as found under :calls in a view.
Each call may carry :headers, :pre-process, :post-process,
:post-process-item and :resource (a resource type string, defaulting to
`knessy--resource-type').
The command line is built with `knessy--kubectl-call->cmd'.
Every process runs in its own fresh buffer.
Each returned `aio' promise resolves to the table parsed by
`knessy--parse-table-kubectl-output'.
The stdout buffer is killed on success if `knessy-shell-kill-success-buffers'
is non-nil.
The promises are in reverse order of CALLS."
  (let ((promises '())
        (resolved '()))
    (dolist (call calls)
      (let ((buf (knessy--utils-fresh-kubectl-buffer "aio-display" t t t))
            (buferr (knessy--utils-fresh-kubectl-buffer "aio-display" t t t t))
            (headers (asoc-get call :headers nil))
            (pre-process-ht (asoc-get call :pre-process (ht)))
            (post-process-ht (asoc-get call :post-process (ht)))
            (post-process-item (asoc-get call :post-process-item nil))
            (knessy--resource-type (asoc-get call :resource knessy--resource-type)))
        ;; TODO: clean this up and teach it various call types
        ;; TODO: continue writing the function forming the cmd string probably
        ;; TODO: dispatch table shouldn't depend on the exec style (sync/async), extract it
        (let ((promise (knessy--shell-exec-aio (knessy--kubectl-call->cmd call)
                                               buf buferr (lambda ()
                                                            (prog1 (knessy--parse-table-kubectl-output
                                                                    buf
                                                                    headers
                                                                    pre-process-ht
                                                                    post-process-ht
                                                                    post-process-item)
                                                              (if knessy-shell-kill-success-buffers
                                                                  (kill-buffer buf)))))))

          (push promise promises))))
    promises))

(defun knessy--kill-success-buffer-maybe (buf)
  "Kill the buffer BUF if `knessy-shell-kill-success-buffers' is non-nil."
  (if knessy-shell-kill-success-buffers
      (kill-buffer buf)))

(defun knessy--perform-calls-sync (calls)
  "Run a kubectl command for each of CALLS synchronously and return results.
CALLS is a list of call alists, as found under :calls in a view.
Each call may carry :headers, :pre-process, :post-process,
:post-process-item and :resource-type (a resource type string, defaulting
to `knessy--resource-type').
The command line is built with `knessy--kubectl-call->cmd'.
Return the list of tables parsed by `knessy--parse-table-kubectl-output',
in reverse order of CALLS.
The output buffers are killed according to
`knessy-shell-kill-success-buffers'."
  (let ((results '()))
    (dolist (call calls)
      (let ((buf (knessy--utils-fresh-kubectl-buffer "display" t t t))
            (headers (asoc-get call :headers nil))
            (pre-process-ht (asoc-get call :pre-process (ht)))
            (post-process-ht (asoc-get call :post-process (ht)))
            (post-process-item (asoc-get call :post-process-item nil))
            (knessy--resource-type (asoc-get call :resource-type knessy--resource-type)))
        (knessy--log 4 (format "in knessy--perform-calls-sync, current buffer: %s" (buffer-name (current-buffer))))
        (knessy--shell-exec (knessy--kubectl-call->cmd call) buf)
        (push (knessy--parse-table-kubectl-output buf headers pre-process-ht post-process-ht post-process-item) results)
        (knessy--kill-success-buffer-maybe buf)))
    results))

(defvar-local knessy--refresh-in-progress nil
  "If T, the buffer refresh is already in progress.
Made so spamming refreshes doesn't result in 100 of kubectl calls.")

(aio-defun knessy--display2-aio ()
  "Refresh the current Knessy buffer asynchronously.
Do nothing if `knessy--refresh-in-progress' is set.
Otherwise set it, unless `knessy--debug' is non-nil.
Start populating the labels cache for the current context, namespace and
resource type.
Run the kubectl calls of the view for the current resource type and view
with `knessy--perform-calls-async', await all of them, and hand the
results to `knessy--display2-common-part' to render the table.
Without a configured view, a single call of the default type is made."
  (knessy--log 3 (format "Display AIO called at %s" (current-time-string)))
  (unless knessy--refresh-in-progress
    (if knessy--debug
        (setq knessy--refresh-in-progress nil)
      (setq knessy--refresh-in-progress t))
    (knessy--log 4 "Refreshing...")
    (let* ((display-buf (current-buffer)))
      (knessy--cache-labels-populate-async knessy--context knessy--namespace knessy--resource-type)
      (let* ((view (ht-get knessy-views (cons knessy--resource-type knessy--view)
                           `((:calls . (((:type . ,knessy-call-default-type)))))))
             (calls (asoc-get view :calls))
             (post-process-fn (asoc-get view :post-process-item nil))
             (results (let ((promises (aio-await (knessy--perform-calls-async calls)))
                            (collected '()))
                          (dolist (promise promises)  ; for some reason, mapcar won't work here. Not a big deal though.
                            (push (aio-await promise) collected))
                          collected)))
        (knessy--display2-common-part display-buf view results post-process-fn)))))

;; from Rosetta Code, https://rosettacode.org/wiki/Pick_random_element
(defun knessy--random-choice (items)
  "Return a randomly chosen element of the list ITEMS.
Return nil if ITEMS is empty."
  (nth (random (length items)) items))

(defcustom knessy-display-start-messages
  '("Working..."
    "Knessy stood at the helm..."
    "Grabbing stuff..."
    "Putting kubectl at work..."
    "Thinking...")
  "List of messages to display when the display command is invoked."
  :group 'knessy
  :type '(repeat string))

;; FIXME: I don't understand why this function is needed, but (commandp 'knessy--display2-aio) is nil?..
(defun knessy--display2 (&optional sync)
  "Refresh the current Knessy buffer with the current environment.
Echo a random message from `knessy-display-start-messages' and push the
current environment on the env ring.
For the linked resource type, call `knessy--display-linked'.
Otherwise call `knessy--display2-sync' if SYNC is non-nil, or start
`knessy--display2-aio' asynchronously if not.
Interactively, SYNC is the raw prefix argument."
  (interactive "P")
  (message (knessy--random-choice knessy-display-start-messages))
  (knessy--env-ring-push (knessy--env))
  (cond ((knessy--resource-type-linked? knessy--resource-type)
         (knessy--display-linked sync))
        (sync
         (knessy--display2-sync))
        (t
         (knessy--display2-aio))))

(comment
 (commandp 'knessy--display2-aio)
 (commandp 'knessy--display2-sync)
 (commandp 'knessy--display2-aio-wrap))

;; TODO: refactor this further, this may be cleaner
(defun knessy--display2-sync ()
  "Refresh the current Knessy buffer, waiting for kubectl to finish.
This is the synchronous counterpart of `knessy--display2-aio'.
Do nothing if `knessy--refresh-in-progress' is set.
Otherwise set it, unless `knessy--debug' is non-nil.
Start populating the labels cache, run the calls of the current view with
`knessy--perform-calls-sync', and render the results with
`knessy--display2-common-part'."
  (interactive)
  (knessy--log 3 (format "Display sync called at %s" (current-time-string)))
  (unless knessy--refresh-in-progress
    (if knessy--debug
        (setq knessy--refresh-in-progress nil)
      (setq knessy--refresh-in-progress t))
    (knessy--log 4 "Refreshing...")
    (let* ((display-buf (current-buffer)))
      (knessy--cache-labels-populate-async knessy--context knessy--namespace knessy--resource-type)
      (let* ((view (ht-get knessy-views (cons knessy--resource-type knessy--view)
                           `((:calls . (((:type . ,knessy-call-default-type)))))))
             (calls (asoc-get view :calls))
             (post-process-fn (asoc-get view :post-process-item nil))
             (results (knessy--perform-calls-sync calls)))
        (knessy--display2-common-part display-buf view results post-process-fn)))))


(defun knessy--display2-common-part (display-buf view results post-process-fn)
  "Merge the RESULTS of the kubectl calls of VIEW and repaint DISPLAY-BUF.
DISPLAY-BUF is the Knessy buffer to repaint.
VIEW is the view alist; its :columns, :column-rename and :widths are used.
RESULTS is the list of parsed tables, one per call.
POST-PROCESS-FN is a function called on every merged item, or nil.
Add a NAMESPACE column before NAME when the current namespace is all,
except for the all-cluster resource type.
If VIEW has no columns, take them from the parsed headers of the results.
Items are merged with `knessy--merge-items'.
Store the resulting headers and items in `knessy--data' of the current
buffer, repaint DISPLAY-BUF, and reset `knessy--refresh-in-progress'."
  (knessy--log 5 "Current buffer name: ")
  (knessy--log 5 (buffer-name))
  (knessy--log 4 "Got calls results!")
  (let* ((columns (asoc-get view :columns nil))
         ;; mix-in NAMESPACE column in front of NAME if current namespace is *ALL*
         ;; if the columns is nil, leave it as is -- NAMESPACE should be present in the kubectl output anyways
         (columns (if (and knessy--namespace-current-all?
                           columns
                           (not (member "NAMESPACE" columns))
                           ;; cluster-scoped resources never have a namespace, so the
                           ;; *ALL-CLUSTER* view deliberately omits this column
                           (not (knessy--resource-type-all-cluster? knessy--resource-type)))
                      (knessy--utils-insert-into-list
                       columns "NAMESPACE" (-elem-index "NAME" columns))
                    columns))
         (column-rename (asoc-get view :column-rename (ht)))
         (widths (ht-merge knessy-column-widths (asoc-get view :widths (ht))))

         (items-calls (mapcar (lambda (x) (asoc-get x :items))
                              results)))
    ;; TODO: knessy mode should be "remembering" widths if adjusted by hand
    ;; (princ "widths:")
    ;; (princ widths)
    ;; TODO: this loop isn't really needed is it?
    ;; setup columns
    (knessy--log 5 "Current buffer name: ")
    (knessy--log 5 (buffer-name))
    (dolist (result results)
      ;; (princ "RESULT: \n")
      ;; (princ result)
      ;; (princ "\n")
      ;; if the view misses columns, most likely it's a default view (so display whatever we got with default call)
      (unless columns
        (setq columns (-> result (asoc-get :headers) (asoc-get :static)))))
    ;; FIXME: this should gather max widths from parsing first, but then merge with configured widths as lowest priority
    ;; (dolist (item (ht-items (-> result (asoc-get :headers) (asoc-get :widths))))
    ;;   (let ((column (car item))
    ;;         (width (cadr item)))
    ;;     (ht-set widths column (max (ht-get widths column 0)
    ;;                                width)))))

    ;; post-process all the items
    ;; TODO: strictly speaking, widths have to be calculated _after_ this -- new columns may appear here!
    ;; TODO: get rid of widths calculation in parsing stage?
    ;; TODO: actually maybe set widths explicitly to avoid reading every single column again?
    (knessy--log 4 "Merging items from multiple calls...")
    (let ((merged-items (apply #'knessy--merge-items items-calls)))
      (when post-process-fn
        (mapc
         (lambda (k)
           (funcall post-process-fn (ht-get merged-items k)))
         (ht-keys merged-items)))
      ;; set the rendering basis datastructure
      (setq knessy--data
            `((:headers . ((:static . ,columns)
                           (:widths . ,widths)
                           (:rename . ,column-rename)))
              (:items . ,merged-items))))
    (knessy--log 5 "Resulting knessy data: ")
    (knessy--log 5 knessy--data)
    ;; paint!
    (knessy--repaint display-buf)
    (knessy--log 5 "Current buffer name: ")
    (knessy--log 5 (buffer-name))
     ;; dumb fix
     ;; (display-buffer display-buf)
    (setq knessy--refresh-in-progress nil)))

(defun knessy--main-buffer-name (base suffix)
  "Return the name of a main Knessy buffer.
BASE is the base name, e.g. `knessy-base-buffer-name'.
SUFFIX is an optional string; if nil or blank, the name is \"*BASE*\",
otherwise \"*BASE - SUFFIX*\"."
  (if (s-blank? suffix)
      (format "*%s*" base)
    (format "*%s - %s*" base suffix)))


(defun knessy-get-new-buffer ()
  "Create and return a fresh buffer for Knessy.
The name is generated from `knessy-base-buffer-name' and made unique.
The buffer name is pushed onto `knessy-buffer-list'.
The buffer is not displayed and its mode is not set."
  (let* ((buf-name (generate-new-buffer-name
                    (knessy--main-buffer-name knessy-base-buffer-name nil)))
         (buf (knessy--utils-make-buffer buf-name)))
    (push buf-name knessy-buffer-list)
    buf))

(defvar knessy-buffer-list nil
  "The list of Knessy buffers.")


;; TODO: add knessy-rename and knessy-list-buffers and maybe knessy-prev-next (how to maintain order?)
(defun knessy-new ()
  "Create new knessy buffer.
Create the directory `knessy-temp-file-dir' if needed.
Enable `knessy-mode' in the new buffer and switch to it.
Initialize its environment from the last selected context, namespace and
resource type, and reset the env ring.
The buffer is not populated with resources."
  (interactive)
  (let* ((knessy-buffer (knessy-get-new-buffer)))
    ;; TODO (pgu, 17.05.2026): huh, should there be a setup step that's run only once or something?
    (make-directory knessy-temp-file-dir t)
    ;; FIXME: is it wrong use of nconc there?
    (setq knessy-buffer-list (nconc knessy-buffer-list (list knessy-buffer)))
    (set-buffer knessy-buffer)
    (knessy-mode)
    (switch-to-buffer knessy-buffer)
    (knessy--set-env-last-selected)
    (knessy--env-ring-reset)))

(defun knessy ()
  "Enable `knessy-mode' in the current buffer.
Use `knessy-new' to create a fresh Knessy buffer instead."
  (interactive)
  (knessy-mode))

;; TODO (pgu, 17.05.2026): separate user-facing knessy--select-context,
;; knessy--select-buffer and the likes into 2 parts: 1) user-facing one, with
;; (completing-read) and all that; 2) internal one, that for instance resets the
;; view when the resource type is changed

(define-derived-mode knessy-mode tabulated-list-mode "Knessy"
  "Mode for Knessy buffers.
Disable undo, start populating the caches asynchronously with
`knessy--caches-populate-async', and set up a buffer-local mode line
entry.
The entry shows context, namespace and resource type, plus the view,
the linked root, label and field selectors and the regex, when set."
  (buffer-disable-undo)
  ;; TODO (pgu, 22.05.2026): (knessy--caches-populate-async) should only populate them if needed
  (knessy--caches-populate-async)
  ;; FIXME: this actually breaks stuff, global hook -- let's override revert for Knessy buffers?
  ;; (add-hook 'tabulated-list-revert-hook #'knessy--display-aio)

  ;; FIXME: hack to work around my kubel hacks
  ;; `setq-local': `mode-line-misc-info' is not buffer-local by default, a plain
  ;; `setq' replaced it for all the buffers, `global-mode-string' included
  (setq-local mode-line-misc-info '())
  (add-to-list 'mode-line-misc-info
               '(:eval
                 (when (eq 'knessy-mode (knessy--buffer-mode))
                   (concat
                    (format "%s/%s/%s"
                            knessy--context
                            knessy--namespace
                            (knessy--resource-type-short knessy--resource-type knessy--context))
                    (if (s-equals? knessy-default-view-string knessy--view)
                        ""
                      (s-concat " (" knessy--view ")"))
                    (if (and knessy--linked-root
                             (equal knessy--resource-type knessy-linked-resource-type-string))
                        (s-concat " <- " (knessy--linked-root->str knessy--linked-root knessy--context))
                      "")
                    (if knessy--label-selectors
                        (s-concat " " (knessy--utils-alist->str-= knessy--label-selectors))
                      "")
                    (if knessy--field-selectors
                        (s-concat " " (knessy--utils-alist->str-= knessy--field-selectors))
                        "")
                    (if (s-blank? knessy--regex)
                        ""
                      (format " /%s/" knessy--regex)))))))


;; TODO: save to a temp file
;; run kubectl apply -f file with namespace context config etc
;; reopen again if refused with error
;; close the buffer

;; TODO: ask user to save if needed
(defun knessy-apply ()
  "Apply the manifest file visited by the current buffer with kubectl.
If the buffer is modified, ask to save it, and abort with a `user-error'
if the answer is no.
Validate the file with `knessy--validate'; if it is valid, apply it
synchronously using the current `knessy--context' and `knessy--namespace',
then kill the buffer and delete the file.
If validation fails, the buffer and the file are kept."
  (interactive)
  (let* ((filename (buffer-file-name))
         (buf-apply (knessy--utils-fresh-kubectl-buffer (s-concat "apply_" filename) t t t)))
    (when (buffer-modified-p)
      (if (y-or-n-p "Buffer was modified. Save and continue? ")
          (save-buffer)
        (user-error "Aborted")))
    (when (knessy--validate)
      (knessy--kubectl-apply-file-sync buf-apply filename)
      ;; TODO (pgu, 17.05.2026): add toggles for deleting/killing buffer behaviour
      (kill-buffer)
      (delete-file filename)
      (message "Applied!"))))

(defun knessy--kubectl-file-sync-arbitrary (verb kubectl-fn prompt-fmt success-msg)
  "Run KUBECTL-FN on the file backing the current buffer.
VERB names the action in the kubectl buffer name (e.g. \"apply\").
KUBECTL-FN is a function of a buffer and a file name, such as
`knessy--kubectl-apply-file-sync'; it is called with the output buffer and
the file name.
PROMPT-FMT is the confirmation prompt format with two %s slots for
context and namespace.
SUCCESS-MSG is the message echoed after KUBECTL-FN is called.
The context, namespace and resource type are the last selected ones.
If the buffer is modified, ask to save it.
Ask for confirmation, then validate the file with `knessy--validate'.
Signal a `user-error' if the user declines either question.
The buffer and file are kept."
  (let* ((knessy--context knessy--last-selected-context)
         (knessy--namespace knessy--last-selected-namespace)
         ;; FIXME (pgu, 18.05.2026): this one is just for the buffer naming really
         (knessy--resource-type knessy--last-selected-resource-type)
         (filename (buffer-file-name))
         (buf (knessy--utils-fresh-kubectl-buffer (s-concat verb "_" filename) t t t)))
    (when (buffer-modified-p)
      (if (y-or-n-p "Buffer was modified. Save and continue? ")
          (save-buffer)
        (user-error "Aborted")))
    (unless (y-or-n-p (format prompt-fmt knessy--context knessy--namespace))
      (user-error "Aborted"))
    (when (knessy--validate)
      (funcall kubectl-fn buf filename)
      ;; TODO (pgu, 17.05.2026): add toggles for deleting/killing buffer behaviour
      (message success-msg))))

(defun knessy-apply-arbitrary ()
  "Apply the manifest file visited by the current buffer, after confirming.
Use the last selected context and namespace, which the prompt shows.
See `knessy--kubectl-file-sync-arbitrary'.
Unlike `knessy-apply', the buffer and the file are kept afterwards."
  (interactive)
  (knessy--kubectl-file-sync-arbitrary
   "apply" #'knessy--kubectl-apply-file-sync
   "Applying to %s, namespace %s, continue? " "Applied!"))

(defun knessy-delete-arbitrary ()
  "Delete the objects in the manifest file visited by the current buffer.
Ask for confirmation first; the prompt shows the last selected context and
namespace.
See `knessy--kubectl-file-sync-arbitrary'.
The buffer and the file are kept afterwards."
  (interactive)
  (knessy--kubectl-file-sync-arbitrary
   "delete" #'knessy--kubectl-delete-file-sync
   "Deleting from %s, namespace %s, continue? " "Deleted!"))

(defun knessy--validate ()
  "Validate the file visited by the current buffer with kubectl.
Use `knessy--kubectl-validate-file-sync', a server-side dry run, with the
current `knessy--context' and `knessy--namespace'.
Return t if kubectl exits with status 0.
Otherwise echo the kubectl output as a message and return nil."
  (let* ((filename (buffer-file-name))
         (buf-validate (knessy--utils-fresh-kubectl-buffer (s-concat "validate_" filename) t t t))
         (exit-code (knessy--kubectl-validate-file-sync buf-validate filename)))
    (if (not (= 0 exit-code))
        (progn
          (message (with-current-buffer buf-validate
                     (buffer-string)))
          nil)
      t)))

(defun knessy-close ()
  "Discard the manifest in the current buffer without applying it.
Kill the buffer and delete the file it visits, without asking.
Echo \"Canceled!\"."
  (interactive)
  (let* ((filename (buffer-file-name)))
    (kill-buffer)
    (delete-file filename)
    (message "Canceled!")))

(defvar knessy-yaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'knessy-apply)
    (define-key map (kbd "C-c C-k") 'knessy-close)
    map)
  "Keymap for `knessy-yaml-mode'.")

(defvar knessy-json-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'knessy-apply)
    (define-key map (kbd "C-c C-k") 'knessy-close)
    map)
  "Keymap for `knessy-json-mode'.")

;; TODO (pgu, 18.05.2026): sentinel echoes error on exit
(defun knessy-log-close ()
  "Kill the current Knessy log buffer."
  (interactive)
  (kill-buffer))

(defun knessy-describe-close ()
  "Kill the current Knessy describe buffer."
  (interactive)
  (kill-buffer))

(defvar knessy-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'knessy-log-close)
    map)
  "Keymap for `knessy-log-mode'.")

(defvar knessy-describe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'knessy-describe-close)
    map)
  "Keymap for `knessy-describe-mode'.")

;; FIXME: no hooks run from yaml-ts-mode?.. keybindings are missing
;; also, are yaml-ts-mode and json-ts-mode standard and default now?
(define-derived-mode knessy-yaml-mode yaml-mode "Knessy-YAML"
  "Mode for Knessy buffers with YAML objects.")

(define-derived-mode knessy-json-mode json-ts-mode "Knessy-JSON"
  "Mode for Knessy buffers with JSON objects.")

(define-derived-mode knessy-log-mode fundamental-mode "Knessy-Log"
  "Mode for Knessy buffers with logs."
  (read-only-mode))

(define-derived-mode knessy-describe-mode text-mode "Knessy-Desc"
  "Mode for Knessy buffers with object descriptions."
  (read-only-mode))

;; FIXME: my personal hacks
(add-hook
 'knessy-mode-hook
 (lambda ()
   (when (and (boundp 'display-line-numbers) display-line-numbers)
     (display-line-numbers-mode -1))
   (when (and (boundp 'persp-mode) persp-mode)
     (persp-add-buffer (current-buffer)))))

(comment
 (remove-hook
  'tabulated-list-revert-hook #'knessy--display-aio))

(provide 'knessy)
;;; knessy.el ends here
