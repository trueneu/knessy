;; -*- lexical-binding: t; -*-

;; Port forwarding: background `kubectl port-forward' processes, started from
;; and stopped via the "port forwarding" transient (`knessy-port-forward').
;;
;; There is no registry of the forwards: the live processes are the registry.
;; Every forward process carries its description in its plist, and has a
;; buffer of its own (never displayed unless asked to), which holds kubectl's
;; output.

(require 'transient)
(require 's)
(require 'dash)

(declare-function knessy--ids "knessy")
(declare-function knessy--id-ns "knessy-utils")
(declare-function knessy--id-rt "knessy-utils")
(declare-function knessy--id-name "knessy-utils")

(defvar knessy--context)
(defvar knessy-kubectl)
(defvar knessy-kubeconfig)
(defvar knessy-base-buffer-name)

(defconst knessy--port-forward-resource-types
  '("pods" "services" "deployments" "replicasets" "statefulsets" "daemonsets"
    "replicationcontrollers" "jobs")
  "Resource types (without the API group) that `kubectl port-forward' accepts.")

(defconst knessy--port-forward-port-regexp
  (rx bos (? (+ digit)) (? ":") (+ digit) eos)
  "A port spec as `kubectl port-forward' takes it: REMOTE, LOCAL:REMOTE or :REMOTE.")

(defun knessy--port-forward-supported? (resource-type)
  "Return t if one can port-forward to RESOURCE-TYPE, nil otherwise.
RESOURCE-TYPE is a name like \"deployments.apps\"; only the part before
the first dot is checked against `knessy--port-forward-resource-types'."
  (and (member (car (s-split "\\." resource-type)) knessy--port-forward-resource-types)
       t))

(defun knessy--port-forward-parse-ports (input)
  "Return the list of port specs in the string INPUT, separated by whitespace.
Each spec is a string matching `knessy--port-forward-port-regexp'.
Signal a `user-error' if there are none, or one of them is malformed."
  (let ((ports (split-string input)))
    (unless ports
      (user-error "No ports given"))
    (dolist (port ports)
      (unless (string-match-p knessy--port-forward-port-regexp port)
        (user-error "Not a port or a port pair: \"%s\" (expected REMOTE, LOCAL:REMOTE or :REMOTE)" port)))
    ports))

(defun knessy--port-forward-target ()
  "Return the ID \(NS . \(RT . NAME)) of the resource to port-forward to.
This is the row at point in the current `tabulated-list-mode' buffer,
even if other rows are marked.
If `knessy--ids' finds more than one target, show a message saying so.
Signal a `user-error' if there is no resource at point, or its type is
not supported, see `knessy--port-forward-supported?'."
  (when (cdr (knessy--ids))
    (message "Multiple targets, choosing one at point"))
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No resource on this line"))
    (unless (knessy--port-forward-supported? (knessy--id-rt id))
      (user-error "Port forwarding is not supported for %s" (knessy--id-rt id)))
    id))

(defun knessy--port-forwards ()
  "Return the list of live port-forward processes.
These are the processes with a non-nil `knessy-port-forward' property
that are still running."
  (-filter (lambda (proc)
             (and (process-get proc 'knessy-port-forward)
                  (process-live-p proc)))
           (process-list)))

(defun knessy--port-forward-buffer-live? (buffer-name)
  "Return non-nil if the buffer named BUFFER-NAME belongs to a running forward.
Return nil if there is no such buffer, or it has no process, or the
process is not a live port-forward."
  (when-let* ((buf (get-buffer buffer-name))
              (proc (get-buffer-process buf)))
    (and (process-get proc 'knessy-port-forward)
         (process-live-p proc))))

(defun knessy--port-forward-announce (proc string)
  "Tell the user which local ports PROC is listening on, if STRING says so.
PROC is a port-forward process and STRING is a chunk of its output.
kubectl prints that when a forward is ready, which is also the only way to
learn the local port picked for a `:REMOTE' spec.
For every \"Forwarding from 127.0.0.1:LOCAL -> REMOTE\" in STRING, show a
message with the description of PROC."
  (let ((start 0))
    (while (string-match "Forwarding from 127\\.0\\.0\\.1:\\([0-9]+\\) -> \\([0-9]+\\)" string start)
      (message "Port-forward %s: localhost:%s -> %s"
               (process-get proc 'knessy-port-forward)
               (match-string 1 string) (match-string 2 string))
      (setq start (match-end 0)))))

(defun knessy--port-forward-filter (proc string)
  "Process filter for port-forward processes: record the output of PROC.
STRING is the output received.
Append STRING at the end of the buffer of PROC, if it is still live,
then call `knessy--port-forward-announce'."
  (when-let* ((buf (process-buffer proc))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert string))))
  (knessy--port-forward-announce proc string))

(defun knessy--port-forward-sentinel (proc _event)
  "Process sentinel for port-forward processes.
PROC is the process; the second argument, the event string, is unused.
Do nothing while PROC is still live.
Otherwise, if PROC was stopped by the user \(its
`knessy-port-forward-stopped' property is set), kill its buffer.
If not, show a message that the forward ended, with its status and the
name of its buffer."
  (unless (process-live-p proc)
    (let ((buf (process-buffer proc)))
      (if (process-get proc 'knessy-port-forward-stopped)
          (when (buffer-live-p buf)
            (kill-buffer buf))
        (message "Port-forward %s ended (%s), see buffer %s"
                 (process-get proc 'knessy-port-forward)
                 (process-status proc)
                 (and (buffer-live-p buf) (buffer-name buf)))))))

(defun knessy--port-forward-start (ctx ns rt name ports)
  "Start `kubectl port-forward' to RT/NAME in the background.
  The resource is in namespace NS of context CTX.
CTX, NS, RT and NAME are strings: the kubectl context, the namespace, the
resource type and the resource name.
PORTS is a list of port specs, strings.
Use `knessy-kubectl' as the command, with `knessy-kubeconfig' as
KUBECONFIG.
The output of the process, stderr included, goes into a new buffer, which
is not displayed.
The process is not queried on exit, and has the filter
`knessy--port-forward-filter' and the sentinel
`knessy--port-forward-sentinel'.
Its `knessy-port-forward' property is set to a description string.
Return the process."
  (let* ((description (format "%s/%s/%s/%s %s" ctx ns rt name (s-join " " ports)))
         (buf (generate-new-buffer
               (format "*%s-port-forward %s*" knessy-base-buffer-name description)))
         (command (append (split-string-and-unquote knessy-kubectl)
                          (list "--context" ctx "-n" ns "port-forward"
                                (format "%s/%s" rt name))
                          ports))
         proc)
    (with-environment-variables (("KUBECONFIG" knessy-kubeconfig))
      (setq proc (make-process :name "knessy-port-forward"
                               :command command
                               :buffer buf
                               ;; stderr too is kubectl's chatter, worth keeping
                               :stderr buf
                               :noquery t
                               :filter #'knessy--port-forward-filter
                               :sentinel #'knessy--port-forward-sentinel)))
    (process-put proc 'knessy-port-forward description)
    proc))

(defun knessy-port-forward-start (ports)
  "Port-forward to the resource at point, in the background.
PORTS is a string of port specs, as `kubectl port-forward' takes them:
REMOTE, LOCAL:REMOTE or :REMOTE, any number of them, separated by
whitespace.
Interactively, read PORTS from the minibuffer, after checking that there
is a supported resource at point.
Use `knessy--context' as the context.
Show a message with the description of the forward.
Return the process, see `knessy--port-forward-start'.
Signal a `user-error' if the resource or the ports are not valid."
  (interactive
   (progn
     (knessy--port-forward-target)      ; fail early, before asking for the ports
     (list (read-string "Ports (REMOTE or LOCAL:REMOTE, separated by spaces): "))))
  (let* ((id (knessy--port-forward-target))
         (ports (knessy--port-forward-parse-ports ports))
         (proc (knessy--port-forward-start
                knessy--context (knessy--id-ns id) (knessy--id-rt id) (knessy--id-name id) ports)))
    (message "Port-forwarding %s..." (process-get proc 'knessy-port-forward))
    proc))

(defun knessy--port-forward-candidates ()
  "Return an alist of buffer name to process, for every live port-forward.
The buffer names serve as the descriptions of the forwards for completion."
  (-map (lambda (proc)
          (cons (buffer-name (process-buffer proc)) proc))
        (knessy--port-forwards)))

(defun knessy--port-forward-completion-table (candidates)
  "Return a completion table of the names in CANDIDATES.
CANDIDATES is an alist as returned by `knessy--port-forward-candidates'.
The table's metadata has the category `knessy-port-forward', which
embark can tell apart, and keeps the candidates in their original order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . knessy-port-forward)
                   (display-sort-function . identity))
      (complete-with-action action (-map #'car candidates) string pred))))

(defun knessy--port-forward-read ()
  "Ask the user for one of the live port-forwards, and return its process.
Use `completing-read' with a required match.
Signal a `user-error' if there are no port-forwards running."
  (let ((candidates (knessy--port-forward-candidates)))
    (unless candidates
      (user-error "No port-forwards running"))
    (cdr (assoc (completing-read "Port-forward: "
                                 (knessy--port-forward-completion-table candidates)
                                 nil t)
                candidates))))

(defun knessy--port-forward-stop-process (proc)
  "Kill the kubectl process PROC, and its buffer.
PROC is a port-forward process.
Mark it as stopped, so that its sentinel kills the buffer, kill the
process, and show a message."
  (process-put proc 'knessy-port-forward-stopped t)
  (kill-process proc)
  (message "Stopped port-forward %s" (process-get proc 'knessy-port-forward)))

(defun knessy-port-forward-stop ()
  "Stop a port-forward, chosen among the running ones.
Ask for the forward with `knessy--port-forward-read', then stop it with
`knessy--port-forward-stop-process'."
  (interactive)
  (knessy--port-forward-stop-process (knessy--port-forward-read)))

(defun knessy--port-forward-show-buffer (proc)
  "Display the buffer of the port-forward process PROC.
Return the window used, as `display-buffer' does."
  (display-buffer (process-buffer proc)))

;; Embark actions on the candidates of `knessy-port-forward-stop'; the default
;; action is to stop the forward, so these are the other ones.
(with-eval-after-load 'embark
  (defun knessy--embark-port-forward-stop (name)
    "Stop the port-forward whose buffer is NAME.
NAME is a buffer name string, as in `knessy--port-forward-candidates'.
Do nothing if there is no such live port-forward."
    (when-let* ((proc (cdr (assoc name (knessy--port-forward-candidates)))))
      (knessy--port-forward-stop-process proc)))
  (defun knessy--embark-port-forward-buffer (name)
    "Show the buffer of the port-forward NAME.
NAME is a buffer name string, as in `knessy--port-forward-candidates'.
Do nothing if there is no such live port-forward."
    (when-let* ((proc (cdr (assoc name (knessy--port-forward-candidates)))))
      (knessy--port-forward-show-buffer proc)))
  (defvar-keymap knessy-embark-port-forward-map
    :doc "Embark actions for Knessy port-forward candidates."
    :parent embark-general-map
    "k" #'knessy--embark-port-forward-stop
    "b" #'knessy--embark-port-forward-buffer)
  (add-to-list 'embark-keymap-alist '(knessy-port-forward . knessy-embark-port-forward-map)))

(transient-define-prefix knessy-port-forward ()
  "Port forwarding transient menu.
Bindings:
  f - start a port-forward to the resource at point,
      see `knessy-port-forward-start'.
  s - stop one of the running port-forwards,
      see `knessy-port-forward-stop'."
  ["Port forwarding"
   ("f" "forward the resource at point" knessy-port-forward-start)
   ("s" "stop a port-forward" knessy-port-forward-stop)])

(provide 'knessy-port-forward)
