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
  "T if one can port-forward to RESOURCE-TYPE, a name like \"deployments.apps\"."
  (and (member (car (s-split "\\." resource-type)) knessy--port-forward-resource-types)
       t))

(defun knessy--port-forward-parse-ports (input)
  "List of the port specs in the string INPUT, separated by whitespace.
Signals a `user-error' if there are none, or one of them is malformed."
  (let ((ports (split-string input)))
    (unless ports
      (user-error "No ports given"))
    (dolist (port ports)
      (unless (string-match-p knessy--port-forward-port-regexp port)
        (user-error "Not a port or a port pair: \"%s\" (expected REMOTE, LOCAL:REMOTE or :REMOTE)" port)))
    ports))

(defun knessy--port-forward-target ()
  "The ID (NS . (RT . NAME)) of the resource to port-forward to.
This is the row at point, even if other rows are marked."
  (when (cdr (knessy--ids))
    (message "Multiple targets, choosing one at point"))
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No resource on this line"))
    (unless (knessy--port-forward-supported? (knessy--id-rt id))
      (user-error "Port forwarding is not supported for %s" (knessy--id-rt id)))
    id))

(defun knessy--port-forwards ()
  "The live port-forward processes."
  (-filter (lambda (proc)
             (and (process-get proc 'knessy-port-forward)
                  (process-live-p proc)))
           (process-list)))

(defun knessy--port-forward-buffer-live? (buffer-name)
  "T if the buffer named BUFFER-NAME belongs to a running port-forward."
  (when-let* ((buf (get-buffer buffer-name))
              (proc (get-buffer-process buf)))
    (and (process-get proc 'knessy-port-forward)
         (process-live-p proc))))

(defun knessy--port-forward-announce (proc string)
  "Tell the user which local ports PROC is listening on, if STRING says so.
kubectl prints that when a forward is ready, which is also the only way to learn
the local port picked for a `:REMOTE' spec."
  (let ((start 0))
    (while (string-match "Forwarding from 127\\.0\\.0\\.1:\\([0-9]+\\) -> \\([0-9]+\\)" string start)
      (message "Port-forward %s: localhost:%s -> %s"
               (process-get proc 'knessy-port-forward)
               (match-string 1 string) (match-string 2 string))
      (setq start (match-end 0)))))

(defun knessy--port-forward-filter (proc string)
  (when-let* ((buf (process-buffer proc))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert string))))
  (knessy--port-forward-announce proc string))

(defun knessy--port-forward-sentinel (proc _event)
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
  "Start `kubectl port-forward' to resource RT/NAME in NS of CTX, in the background.
PORTS is a list of port specs. Returns the process."
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
PORTS are as `kubectl port-forward' takes them: REMOTE, LOCAL:REMOTE or :REMOTE,
any number of them, separated by whitespace."
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
  "Alist of a description to the process, for every live port-forward."
  (-map (lambda (proc)
          (cons (buffer-name (process-buffer proc)) proc))
        (knessy--port-forwards)))

(defun knessy--port-forward-completion-table (candidates)
  "Completion table of the names in CANDIDATES, which embark can tell apart."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . knessy-port-forward)
                   (display-sort-function . identity))
      (complete-with-action action (-map #'car candidates) string pred))))

(defun knessy--port-forward-read ()
  "Ask the user for one of the live port-forwards, return its process."
  (let ((candidates (knessy--port-forward-candidates)))
    (unless candidates
      (user-error "No port-forwards running"))
    (cdr (assoc (completing-read "Port-forward: "
                                 (knessy--port-forward-completion-table candidates)
                                 nil t)
                candidates))))

(defun knessy--port-forward-stop-process (proc)
  "Kill the kubectl process PROC, and its buffer."
  (process-put proc 'knessy-port-forward-stopped t)
  (kill-process proc)
  (message "Stopped port-forward %s" (process-get proc 'knessy-port-forward)))

(defun knessy-port-forward-stop ()
  "Stop a port-forward, chosen among the running ones."
  (interactive)
  (knessy--port-forward-stop-process (knessy--port-forward-read)))

(defun knessy--port-forward-show-buffer (proc)
  (display-buffer (process-buffer proc)))

;; Embark actions on the candidates of `knessy-port-forward-stop'; the default
;; action is to stop the forward, so these are the other ones.
(with-eval-after-load 'embark
  (defun knessy--embark-port-forward-stop (name)
    "Stop the port-forward whose buffer is NAME."
    (when-let* ((proc (cdr (assoc name (knessy--port-forward-candidates)))))
      (knessy--port-forward-stop-process proc)))
  (defun knessy--embark-port-forward-buffer (name)
    "Show the buffer of the port-forward NAME."
    (when-let* ((proc (cdr (assoc name (knessy--port-forward-candidates)))))
      (knessy--port-forward-show-buffer proc)))
  (defvar-keymap knessy-embark-port-forward-map
    :doc "Embark actions for Knessy port-forward candidates."
    :parent embark-general-map
    "k" #'knessy--embark-port-forward-stop
    "b" #'knessy--embark-port-forward-buffer)
  (add-to-list 'embark-keymap-alist '(knessy-port-forward . knessy-embark-port-forward-map)))

(transient-define-prefix knessy-port-forward ()
  "Port forwarding"
  ["Port forwarding"
   ("f" "forward the resource at point" knessy-port-forward-start)
   ("s" "stop a port-forward" knessy-port-forward-stop)])

(provide 'knessy-port-forward)
