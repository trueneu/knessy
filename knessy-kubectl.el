;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'knessy-utils)

;; TODO: include labels as filters here?
;; TODO: rename to kubectl get command and fix the verb?..
(defun knessy--kubectl-cmd (verb &optional fmt omit-namespace? no-headers? omit-context? omit-labels? omit-fields?)
"Build a `kubectl' command string for the current view and return it.
VERB is the kubectl verb, e.g. \"get\", \"describe\", \"top\" or \"config\".
FMT, if non-nil, is appended as \" -o FMT\".
OMIT-NAMESPACE?, if non-nil, drops the namespace flags (-n and -A).
NO-HEADERS?, if non-nil, appends \" --no-headers\".
OMIT-CONTEXT?, if non-nil, drops the \" --context\" flag.
OMIT-LABELS?, if non-nil, ignores `knessy--label-selectors'.
OMIT-FIELDS?, if non-nil, ignores `knessy--field-selectors'.
The command is built as
kubectl [--context CTX] [-n NS] VERB RESOURCE-TYPE [-A] [-o FMT]
[--no-headers] [-l LABELS] [--field-selector FIELDS].
Reads the dynamic variables `knessy-kubectl', `knessy--context',
`knessy--namespace', `knessy--resource-type',
`knessy--label-selectors' and `knessy--field-selectors'.
\"-n NS\" is omitted when the namespace is nil or the all-namespaces
marker, in which case \"-A\" is added instead (unless OMIT-NAMESPACE?).
Signal an error if `knessy--resource-type' is one of the synthetic
*ALL-...* or *LINKED* pseudo resource types.
The command is logged at level 3 but not run."
  (when (knessy--resource-type-all-synthetic? knessy--resource-type)
    (error "Refusing to run kubectl with the literal %s resource-type: the view for it is missing or a stale `knessy-views' is still loaded (defcustom values don't reload on `load'/`eval-buffer' once already bound -- restart Emacs, or `(makunbound 'knessy-views)' and reload knessy-views.el)"
           knessy--resource-type))
  (let ((cmd (s-concat
              knessy-kubectl
              (if omit-context?
                  ""
                (s-concat " --context " knessy--context))
              (if (or omit-namespace? (knessy--namespace-all? knessy--namespace) (null knessy--namespace)) ; namespace may be nil if the object is not namespaced
                  ""
                (s-concat " -n " knessy--namespace))
              " " verb " "  knessy--resource-type
              (if (and (not omit-namespace?) (knessy--namespace-all? knessy--namespace))
                  " -A"
                "")
              (if fmt
                  (s-concat " -o " fmt)
                "")
              (if no-headers?
                  " --no-headers"
                "")
              (if (and knessy--label-selectors (not omit-labels?))
                  (s-concat
                   " -l "
                   (knessy--utils-alist->str-= knessy--label-selectors))
                "")
              (if (and knessy--field-selectors (not omit-fields?))
                  (s-concat
                   " --field-selector "
                   (knessy--utils-alist->str-= knessy--field-selectors))
                ""))))
    (knessy--log 3 cmd)
    cmd))

(defun knessy--kubectl-file-cmd (verb filename &optional dry-run)
"Build a `kubectl' command string that runs VERB on a file and return it.
VERB is the kubectl verb, e.g. \"apply\".
FILENAME is the path passed to \"-f\".
DRY-RUN, if non-nil, appends \" --dry-run=server\".
The command is kubectl --context CTX [-n NS] VERB -f FILENAME, where
\"-n NS\" is omitted when the namespace is the all-namespaces marker.
Reads `knessy-kubectl', `knessy--context' and `knessy--namespace'.
The command is logged at level 3 but not run."
  (let ((cmd (s-concat
              knessy-kubectl
              (s-concat " --context " knessy--context)
              (if (knessy--namespace-all? knessy--namespace)
                  ""
                (s-concat " -n " knessy--namespace))
              " " verb " -f "
              filename
              (if dry-run
                  " --dry-run=server"
                ""))))
    (knessy--log 3 cmd)
    cmd))

(defun knessy--kubectl-delete-cmd (filename &optional wait dry-run)
"Build a `kubectl delete -f' command string for a file and return it.
FILENAME is the path passed to \"-f\".
WAIT, if non-nil, appends \" --wait=true\", otherwise \" --wait=false\".
DRY-RUN, if non-nil, appends \" --dry-run=server\".
The command is kubectl --context CTX [-n NS] delete -f FILENAME, where
\"-n NS\" is omitted when the namespace is the all-namespaces marker.
Reads `knessy-kubectl', `knessy--context' and `knessy--namespace'.
The command is logged at level 3 but not run."
  (let ((cmd (s-concat
              knessy-kubectl
              (s-concat " --context " knessy--context)
              (if (knessy--namespace-all? knessy--namespace)
                  ""
                (s-concat " -n " knessy--namespace))
              " delete -f "
              filename
              (if dry-run
                  " --dry-run=server"
                "")
              (if wait
                  " --wait=true"
                " --wait=false"))))
    (knessy--log 3 cmd)
    cmd))

(defun knessy--kubectl-delete-object-cmd (ns resource-type name &optional force? grace-period now?)
"Build a `kubectl delete' command string for one object and return it.
NS is the namespace string; if nil, no \"-n\" flag is added.
RESOURCE-TYPE is the resource type string and NAME the object name;
they are joined as RESOURCE-TYPE/NAME.
FORCE?, if non-nil, adds \" --force --grace-period=0\" and makes
GRACE-PERIOD and NOW? be ignored.
GRACE-PERIOD, if non-nil, is a string appended as \" --grace-period=N\".
NOW?, if non-nil, adds \" --grace-period=1\".
Reads `knessy-kubectl' and `knessy--context'.
The command is logged at level 3 but not run."
  (let ((cmd (s-concat
              knessy-kubectl
              (s-concat " --context " knessy--context)
              (if ns
                  (s-concat " -n " ns)
                "")
              " delete "
              (s-concat resource-type "/" name " ")
              (if force?
                  " --force --grace-period=0"
                "")
              (if (and grace-period (not force?))
                  (s-concat " --grace-period=" grace-period)
                "")
              (if (and now? (not force?))
                  " --grace-period=1"
                ""))))

    (knessy--log 3 cmd)
    cmd))

(defun knessy--kubectl-get-contexts-cmd ()
"Return the command string that lists the kubectl context names.
The result is `kubectl config get-contexts -o name', without
--context, namespace, label or field flags.
Binds `knessy--resource-type' to \"get-contexts\" while building it."
  (let ((knessy--resource-type "get-contexts"))
    (knessy--kubectl-cmd "config" "name" t nil t t t)))

(defun knessy--kubectl-get-namespaces-cmd ()
"Return the command string that lists the namespace names.
The result is kubectl --context CTX get namespaces
-o custom-columns='NAME:.metadata.name' --no-headers.
Reads `knessy--context'.
Binds `knessy--resource-type' to \"namespaces\" and ignores namespace,
label and field selectors."
  (let ((knessy--resource-type "namespaces"))
    (knessy--kubectl-cmd "get" "custom-columns='NAME:.metadata.name'" t t nil t t)))

(defun knessy--kubectl-get-labels-cmd ()
"Return the command string that prints the labels of every object.
The command is a `kubectl get' with a jsonpath output that prints the
`.metadata.labels' of each item on its own line, with --no-headers.
It uses the current `knessy--context', `knessy--namespace' and
`knessy--resource-type', and ignores label and field selectors.
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-get-labels-cmd")
  (knessy--kubectl-cmd "get" "jsonpath='{range .items[*]}{.metadata.labels}{\"\\n\"}{end}'" nil t nil t t))

;; TODO (pgu, 17.05.2026): include resource-type as explicit argument here, to support multiple resource-type views
(defun knessy--kubectl-get-obj-cmd (ns rt name &optional fmt)
"Return the command string that gets a single object.
NS is the namespace string and RT the resource type string.
NAME is the object name; the command targets RT/NAME.
FMT is the output format: :json or nil gives \"-o json\", :yaml gives
\"-o yaml\", and any other value adds no \"-o\" flag.
Binds `knessy--namespace' and `knessy--resource-type' while building,
and reads `knessy--context'.
Label and field selectors are ignored.
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-get-obj-cmd")
  (let ((knessy--resource-type (s-concat rt "/" name))
        (knessy--namespace ns))
    (knessy--kubectl-cmd "get" (cond ((or (eq fmt :json)
                                          (null fmt))
                                      "json")
                                     ((eq fmt :yaml)
                                      "yaml")) nil nil nil t t)))

(comment
 (not "abc"))

(defun knessy--kubectl-log-cmd (ns resource-type name &optional follow? tail parallel labels all-containers? prefix? container timestamps?)
"Build a `kubectl logs' command string and return it.
NS is the namespace string, RESOURCE-TYPE the resource type and NAME
the object name; the target is RESOURCE-TYPE/NAME.
FOLLOW?, if non-nil, adds \" -f\".
TAIL, if non-nil, is a string appended as \" --tail N\".
PARALLEL, if non-nil, is an integer appended as \" --max-log-requests N\".
LABELS is currently unused.
ALL-CONTAINERS?, if non-nil, adds \" --all-containers\" and CONTAINER
is then ignored.
PREFIX?, if non-nil, adds \" --prefix\".
CONTAINER, if non-nil and ALL-CONTAINERS? is nil, adds \" -c CONTAINER\".
TIMESTAMPS?, if non-nil, adds \" --timestamps\".
Reads `knessy-kubectl' and `knessy--context'.
The command is not run.
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-log-cmd")

  (let* ((object (s-concat resource-type "/" name))) ;; it's either name, or labels, not both

    (s-concat
     knessy-kubectl
     " --context " knessy--context
     " -n " ns
     " logs " object
     (if follow?
         " -f"
       "")
     (if parallel
         (s-concat " --max-log-requests " (int-to-string parallel))
       "")
     (if prefix?
         (s-concat " --prefix")
       "")
     (if all-containers?
         " --all-containers"
       (if container
           (s-concat " -c " container)
         ""))
     (if tail
         (s-concat " --tail " tail)
       "")
     (if timestamps?
         (s-concat " --timestamps")
       ""))))

(comment
 (knessy--kubectl-log-cmd "pods" "abc" t 50 5 nil t t nil t)
 (let* ((buf (get-buffer-create "test1"))
        (buferr (get-buffer-create "test1err"))
        (knessy--context "k8s-local")
        (knessy--namespace "default")
        (knessy--resource-type "pods"))
   (knessy--shell-exec-async3
    (knessy--kubectl-log-cmd knessy--resource-type "hello-loop-pod" t 10 nil nil nil nil nil t)
    buf
    buferr
    (knessy--make-process-filter-logs buf "blah" t)
    (lambda () (message "callback called")))
   (knessy--shell-exec-async3
    (knessy--kubectl-log-cmd knessy--resource-type "hello-loop-pod2" t 10 nil nil nil nil nil t)
    buf
    buferr
    (knessy--make-process-filter-logs buf "blah" t)
    (lambda () (message "callback called")))
   (with-current-buffer buf
     (knessy-log-mode))))

;; (defun knessy--kubectl-log)

;; TODO (pgu, 17.05.2026): include resource-type as explicit argument here, to support multiple resource-type views
(defun knessy--kubectl-describe-obj-cmd (ns rt name)
"Return the command string that describes a single object.
NS is the namespace string and RT the resource type string.
NAME is the object name; the command targets RT/NAME.
The result is `kubectl --context CTX -n NS describe RT/NAME'.
Binds `knessy--namespace' and `knessy--resource-type' while building,
and reads `knessy--context'.
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-describe-obj-cmd")
  (let ((knessy--resource-type (s-concat rt "/" name))
        (knessy--namespace ns))
    (knessy--kubectl-cmd "describe" nil nil nil nil t t)))

(defun knessy--kubectl-apply-file-cmd (filename)
"Return the command string that applies the manifest file FILENAME.
The result is built with `knessy--kubectl-file-cmd' and the verb
\"apply\", using the current `knessy--context' and `knessy--namespace'.
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-apply-file-cmd")
  (knessy--kubectl-file-cmd "apply" filename))

(defun knessy--kubectl-validate-file-cmd (filename)
"Return the command string that validates the manifest file FILENAME.
This is the \"apply\" command with the server-side dry run flag, built
with `knessy--kubectl-file-cmd', so nothing is changed in the cluster.
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-validate-file-cmd")
  (knessy--kubectl-file-cmd "apply" filename t))

(defun knessy--kubectl-delete-file-cmd (filename)
"Return the command string that deletes the objects in FILENAME.
Built with `knessy--kubectl-delete-cmd' without waiting, so the
command ends with \"--wait=false\".
Logs a message at level 4."
  (knessy--log 4 "called knessy--kubectl-delete-file-cmd")
  (knessy--kubectl-delete-cmd filename nil))

(comment
 (let ((knessy--context "k8s-local")
       (knessy--namespace "kube-system")
       (knessy--resource-type "pods")
       (name "traefik-c5c8bf4ff-crlxs"))
   (knessy--kubectl-get-obj-cmd name :yaml)))

(defun knessy--kubectl-call->cmd (call)
"Return the `kubectl' command string for the view call CALL.
CALL is an alist read with `asoc-get'; its :type selects the command:
:get-wide and :get build a \"get\" (the former with \"-o wide\"),
:custom-columns and :jsonpath build a \"get\" whose output format is
built from the :spec entry, :top builds a \"top\", and
:all-resources builds a jsonpath \"get\" over every listable resource
type of the :scope entry, which is :cluster for cluster-scoped
resource types or anything else for namespaced ones.
For :all-resources the namespace flags and the label and field
selectors are omitted.
Return nil if :type is not one of these.
Commands use the dynamic variables `knessy--context',
`knessy--namespace', `knessy--resource-type' and the selectors, via
`knessy--kubectl-cmd'.
For :all-resources the api-resources cache of the current context is
consulted, which may run kubectl."
  (let ((type (asoc-get call :type)))
    (cond ((eq :get-wide type)
           (knessy--kubectl-cmd "get" "wide"))
          ((eq :get type)
           (knessy--kubectl-cmd "get"))
          ((eq :custom-columns type)
           (knessy--kubectl-cmd "get" (s-concat "custom-columns=" (asoc-get call :spec))))
          ((eq :jsonpath type)
           (knessy--kubectl-cmd "get" (s-concat "jsonpath=" (asoc-get call :spec))))
          ((eq :all-resources type)
           ;; query every listable resource type of the given :scope at once;
           ;; labels/fields are per-resource-type and don't apply uniformly
           ;; across all kinds
           (let* ((scope (asoc-get call :scope))
                  (cluster? (eq scope :cluster))
                  (knessy--resource-type (if cluster?
                                             (knessy--kubectl-all-cluster-resource-types-joined)
                                           (knessy--kubectl-all-namespaced-resource-types-joined))))
             (knessy--kubectl-cmd "get" (s-concat "jsonpath=" (asoc-get call :spec)) cluster? nil nil t t)))
          ((eq :top type)
           (knessy--kubectl-cmd "top")))))

(defun knessy--kubectl-contexts ()
"Run kubectl synchronously and return the list of context names.
The result is a list of strings, built by `knessy--utils-read-buffer'
from the output lines, which makes it be in reverse order of the output.
Output goes to a freshly created temporary buffer, which is killed
after reading."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "contexts" t t t)))))
    (knessy--shell-exec
     (knessy--kubectl-get-contexts-cmd)
     buf)
    (knessy--utils-read-buffer buf)))

(defun knessy--kubectl-namespaces ()
"Run kubectl synchronously and return the namespace names.
The result is a list of strings, built by `knessy--utils-read-buffer'
from the output lines, which makes it be in reverse order of the output.
Uses the current `knessy--context'.
Output goes to a freshly created temporary buffer, which is killed
after reading."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "namespaces" nil t t)))))
    (knessy--shell-exec
     (knessy--kubectl-get-namespaces-cmd)
     buf)
    (knessy--utils-read-buffer buf)))

(defun knessy--kubectl-labels ()
"Run kubectl synchronously and return the labels of the current view.
Uses `knessy--context', `knessy--namespace' and
`knessy--resource-type' via `knessy--kubectl-get-labels-cmd'.
Each output line is parsed as a JSON object, and the objects are merged
by `knessy--utils-ht-merge-duplicates-to-sets' into a hash table of
label names to sets of values seen.
Unlike `knessy--cache-labels-populate-async', empty lines are not
handled specially.
Output goes to a freshly created buffer, which is not killed."
  (let ((buf (knessy--utils-make-buffer
              (generate-new-buffer-name
               (knessy--utils-kubectl-buffer-name "labels")))))
    (knessy--shell-exec (knessy--kubectl-get-labels-cmd) buf)
    (apply
     #'knessy--utils-ht-merge-duplicates-to-sets
     (mapcar
      (lambda (s) (json-parse-string s))
      (knessy--utils-read-buffer buf t)))))

(comment
 (let ((knessy--context "minikube")
       (knessy--namespace "kube-system")
       (knessy--resource-type "pods"))
   (knessy--kubectl-labels)
   (knessy--cache-get knessy--cache (list :labels knessy--context knessy--namespace knessy--resource-type) #'knessy--kubectl-labels
                      knessy-label-cache-ttl t))
 (-> knessy--cache
     (ht-get :labels)
     (ht-get "minikube")
     (ht-get "kube-system")
     (ht-get "pods")))

(defun knessy--namespaces ()
"Return the cached list of namespaces of the current context.
The list starts with `knessy-all-namespaces-string', followed by the
namespace names from `knessy--kubectl-namespaces'.
The cache entry (:namespaces `knessy--context') in `knessy--cache' is
refreshed by running kubectl synchronously when missing or expired."
  (knessy--cache-get
   knessy--cache
   (list :namespaces knessy--context)
   (lambda ()
     (cons knessy-all-namespaces-string
       (knessy--kubectl-namespaces)))))

;; Every "which resource types are X" question below (all of them, namespaced
;; ones, cluster-scoped ones, listable ones...) is answered from a single
;; `kubectl api-resources -o json' call per context, cached as-is; each
;; accessor is then just a pure filter/map over that one cached list instead
;; of a kubectl call of its own.
(defun knessy--kubectl-api-resources-cmd ()
"Return the command string that lists the API resources as JSON.
The result is `kubectl --context CTX api-resources -o json', using the
current `knessy--context'."
  (s-concat knessy-kubectl
            " --context " knessy--context
            " api-resources -o json"))

(defun knessy--kubectl-api-resources-parse (buf)
"Parse BUF into a list of resource type alists and return it.
BUF is a buffer holding the output of `kubectl api-resources -o json';
it is read from its start and is left alive.
Each alist is shaped like
`((:name . FULLY-QUALIFIED-NAME) (:kind . KIND) (:group . GROUP)
  (:short-names . (SHORT-NAME ...)) (:namespaced . BOOL)
  (:verbs . (VERB ...)))'.
GROUP is an empty string for the core API group.
FULLY-QUALIFIED-NAME matches what `kubectl api-resources --output name'
prints, e.g. \"deployments.apps\", or just \"nodes\" for core resources."
  (let* ((parsed (with-current-buffer buf
                   (goto-char (point-min))
                   (json-parse-buffer :object-type 'alist :array-type 'list)))
         (resources (alist-get 'resources parsed)))
    (mapcar
     (lambda (r)
       (let* ((name (alist-get 'name r))
              (group (alist-get 'group r)))
         `((:name . ,(if group (s-concat name "." group) name))
           (:kind . ,(alist-get 'kind r))
           (:group . ,(or group ""))
           (:short-names . ,(alist-get 'shortNames r))
           (:namespaced . ,(eq t (alist-get 'namespaced r)))
           (:verbs . ,(alist-get 'verbs r)))))
     resources)))

(defun knessy--kubectl-api-resources-list ()
"Run and parse `kubectl api-resources' once and return the result.
Uses the current `knessy--context'.
The command runs synchronously into a temporary buffer, which is
killed afterwards.
Return the list of alists produced by `knessy--kubectl-api-resources-parse'."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "api-resources" nil t t)))))
    (knessy--shell-exec (knessy--kubectl-api-resources-cmd) buf)
    (prog1 (knessy--kubectl-api-resources-parse buf)
      (kill-buffer buf))))

(defun knessy--api-resources ()
"Return the cached api-resources list of the current context.
The list is that of `knessy--kubectl-api-resources-parse'.
The cache entry (:api-resources `knessy--context') in `knessy--cache' is
refreshed by running kubectl synchronously when missing or expired."
  (knessy--cache-get
   knessy--cache
   (list :api-resources knessy--context)
   (lambda ()
     (knessy--kubectl-api-resources-list))))

(defcustom knessy-use-short-resource-type-names t
  "If non-nil, show short resource type names (e.g. \"deploy\") where Knessy
displays them; if nil, always show the full names.
The short names are still fetched and parsed either way."
  :type 'boolean
  :group 'knessy)

(defcustom knessy-resource-type-short-names '()
  "Alist of fully qualified resource type name to the short name to display.
Takes precedence over the short names `kubectl api-resources' reports, e.g.
`((\"deployments.apps\" . \"dep\"))'."
  :type '(alist :key-type string :value-type string)
  :group 'knessy)

(defun knessy--api-resource (resource-type &optional context)
"Return the api-resources entry for RESOURCE-TYPE, or nil.
RESOURCE-TYPE is a fully qualified resource type name string.
CONTEXT is the context name; nil means `knessy--context'.
Only the cache is looked at and kubectl is never run: if the
api-resources of CONTEXT are not cached and fresh, return nil."
  (let ((keys (list :api-resources (or context knessy--context))))
    (and (knessy--cache-fresh-p knessy--cache keys)
         (-find (lambda (r) (equal (asoc-get r :name) resource-type))
                (knessy--cache-get knessy--cache keys #'ignore)))))

(defun knessy--resource-type-short (resource-type &optional context)
"Return a short display name for RESOURCE-TYPE, for display only.
CONTEXT is the context name; nil means `knessy--context'.
Look, in order, at `knessy-resource-type-short-names', the first short
name kubectl reports for the type, and the type name with its API group
stripped.
Use the api-resources cached for CONTEXT and never run kubectl: if
they are not cached, or the type is unknown, return RESOURCE-TYPE
unchanged.
Always return RESOURCE-TYPE unchanged if
`knessy-use-short-resource-type-names' is nil."
  (if (not knessy-use-short-resource-type-names)
      resource-type
    (let ((resource (knessy--api-resource resource-type context)))
      (or (asoc-get knessy-resource-type-short-names resource-type)
          (and resource
               (or (car (asoc-get resource :short-names))
                   (let ((group (asoc-get resource :group)))
                     (if (s-blank? group)
                         resource-type
                       (s-chop-suffix (s-concat "." group) resource-type)))))
          resource-type))))

(defun knessy--resource-types ()
"Return the list of resource type names available in the current context.
The list starts with `knessy-all-namespaced-resource-types-string' and
`knessy-all-cluster-resource-types-string', followed by the fully
qualified names from `knessy--api-resources'."
  (cons knessy-all-namespaced-resource-types-string
        (cons knessy-all-cluster-resource-types-string
              (mapcar (lambda (r) (asoc-get r :name)) (knessy--api-resources)))))

(defun knessy--resource-types-set ()
"Return a hash set of all resource type names of the current context.
Names come from `knessy--api-resources'; the synthetic *ALL-...*
entries are not included.
The set is built with `knessy--utils-set', so names map to t."
  (knessy--utils-set
   (mapcar (lambda (r) (asoc-get r :name)) (knessy--api-resources))))

(defun knessy--resource-types-namespaced ()
"Return a hash set of the namespaced resource type names.
Names come from `knessy--api-resources' of the current context.
The set is built with `knessy--utils-set', so names map to t."
  (knessy--utils-set
   (->> (knessy--api-resources)
        (-filter (lambda (r) (asoc-get r :namespaced)))
        (mapcar (lambda (r) (asoc-get r :name))))))

;; only resource types supporting the "list" verb can be safely combined into
;; a single "kubectl get type1,type2,..." call without the whole call failing
(defun knessy--resource-types-listable-namespaced ()
"Return the list of namespaced resource type names that support \"list\".
Names come from `knessy--api-resources' of the current context."
  (->> (knessy--api-resources)
       (-filter (lambda (r) (and (asoc-get r :namespaced)
                                 (member "list" (asoc-get r :verbs)))))
       (mapcar (lambda (r) (asoc-get r :name)))))

(defun knessy--resource-types-listable-cluster ()
"Return the list of cluster-scoped resource type names that support \"list\".
Names come from `knessy--api-resources' of the current context."
  (->> (knessy--api-resources)
       (-filter (lambda (r) (and (not (asoc-get r :namespaced))
                                 (member "list" (asoc-get r :verbs)))))
       (mapcar (lambda (r) (asoc-get r :name)))))

(defun knessy--kubectl-all-namespaced-resource-types-joined ()
"Return a comma-joined string of every listable namespaced resource type.
It is meant for the *ALL-NS* view, as the resource type of a single
`kubectl get' call.
See `knessy--resource-types-listable-namespaced'."
  (s-join "," (knessy--resource-types-listable-namespaced)))

(defun knessy--kubectl-all-cluster-resource-types-joined ()
"Return a comma-joined string of every listable cluster-scoped resource type.
It is meant for the *ALL-CLUSTER* view, as the resource type of a single
`kubectl get' call.
See `knessy--resource-types-listable-cluster'."
  (s-join "," (knessy--resource-types-listable-cluster)))

(defun knessy-cache-clear ()
"Reset all the Knessy caches.
Clear the hash table `knessy--cache'.
Nothing is repopulated; see `knessy-caches-refresh' for that."
  (interactive)
  (ht-clear knessy--cache))

;; TODO: things different between the two functions below are
;; buffer names
;; kubectl commands
;; destinations
(defun knessy--cache-namespaces-populate-async ()
"Start fetching the namespaces of every context that has none cached.
Contexts come from `knessy--contexts'; those with a fresh
\(:namespaces CTX) entry in `knessy--cache' are skipped.
For each remaining context, an asynchronous kubectl process is started.
When it finishes, a message is shown and the cache entry is set to
`knessy-all-namespaces-string' followed by the namespace names.
Output and error buffers are created per context; the output buffer
is read, and so killed, by `knessy--utils-read-buffer'.
Return value is not meaningful."
  (dolist (ctx (cl-remove-if
                (lambda (ctx) (knessy--cache-fresh-p knessy--cache (list :namespaces ctx)))
                (knessy--contexts)))
    (let ((buf (knessy--utils-make-buffer
                (generate-new-buffer-name
                 (knessy--utils-kubectl-buffer-name "namespace-cache" nil t t))))
          (buferr (knessy--utils-make-buffer
                   (generate-new-buffer-name
                    (knessy--utils-kubectl-buffer-name "namespace-cache" nil t t t)))))

      (knessy--shell-exec-async2
       (concat
        knessy-kubectl
        " --context "
        ctx
        " get namespaces --output custom-columns=NAME:.metadata.name --no-headers")
       buf
       buferr
       (lambda ()
         (message "Refreshed namespaces for %s" ctx)
         (knessy--cache-set
          knessy--cache
          (list :namespaces ctx)
          (cons knessy-all-namespaces-string (knessy--utils-read-buffer buf))))))))

(comment
 (knessy--cache-resource-types-populate-async))

(defun knessy--cache-resource-types-populate-async ()
"Start fetching the api-resources of every context that has none cached.
Contexts come from `knessy--contexts'; those with a fresh
\(:api-resources CTX) entry in `knessy--cache' are skipped.
For each remaining context, an asynchronous `kubectl api-resources
-o json' process is scheduled to start one second later with
`run-at-time'.
When it finishes, a message is shown, the parsed list is stored in the
cache and the output buffer is killed if
`knessy-shell-kill-success-buffers' says so.
Return value is not meaningful."
  (dolist (ctx (cl-remove-if
                (lambda (ctx) (knessy--cache-fresh-p knessy--cache (list :api-resources ctx)))
                (knessy--contexts)))
    (let* ((knessy--context ctx)
           (buf (knessy--utils-make-buffer
                 (generate-new-buffer-name
                  (knessy--utils-kubectl-buffer-name "api-resources-cache" nil t t))))
           (buferr (knessy--utils-make-buffer
                    (generate-new-buffer-name
                     (knessy--utils-kubectl-buffer-name "api-resources-cache" nil t t t)))))
      ;; TODO: the actual command should not live here
      ;; TODO (pgu, 19.05.2026): hack
      (run-at-time
       1
       nil
       (lambda ()
         (knessy--shell-exec-async2
          (concat knessy-kubectl " --context " ctx " api-resources -o json")
          buf
          buferr
          (lambda ()
            (message "Refreshed API resources for %s" ctx)
            (knessy--cache-set
             knessy--cache
             (list :api-resources ctx)
             (knessy--kubectl-api-resources-parse buf))
            (knessy--kill-success-buffer-maybe buf))))))))



(comment
 (let ((ctx "k8s-local")
       (buf-namespaced (get-buffer-create "knessy-debug"))
       (buferr-namespaced (get-buffer-create "knessy-debug-err")))
   (knessy--shell-exec-async2
    (concat
     knessy-kubectl
     " --context "
     ctx
     " api-resources --namespaced=true --output name")
    buf-namespaced
    buferr-namespaced
    (lambda ()
      (knessy--cache-set
       knessy--cache
       (list :resource-types-namespaced ctx)
       (knessy--utils-set
        (knessy--utils-read-buffer buf-namespaced)))))))


(defun knessy--caches-populate-async ()
"Start an asynchronous refresh of the main caches.
Run `kubectl config get-contexts --output name' in the background.
When it finishes, a message is shown, the list of contexts is stored
in `knessy--cache' under (:ctx), and then
`knessy--cache-namespaces-populate-async' and
`knessy--cache-resource-types-populate-async' are called.
Return value is not meaningful."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "context-cache" t t t))))
        (buferr (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "context-cache" t t t t)))))
    ;; TODO: the actual command should not live here
    (knessy--shell-exec-async2
     ;; TODO (pgu, 19.05.2026): hack to avoid Azure login galore
     (concat knessy-kubectl " config get-contexts --output name")
     buf
     buferr
     (lambda ()
       (message "Refreshed available contexts")
       (knessy--cache-set
        knessy--cache
        '(:ctx)
        (knessy--utils-read-buffer buf))
       ;; TODO (pgu, 19.05.2026): hack
       (knessy--cache-namespaces-populate-async)
       (knessy--cache-resource-types-populate-async)))))

;; FIXME: any async functions should not be dependent on buffer-local variables because the user may
;;   change the current buffer while the function is running :D
;;   or at least capture those early enough.
;;   or pass those explicitly at the call time.

(defun knessy--kubectl-parse-json-buffer (buf)
"Parse the JSON contents of BUF and return the result.
BUF is a buffer; parsing starts from its beginning and the buffer is
left alive, with point moved after the parsed value.
Use the defaults of `json-parse-buffer', so objects become hash tables."
  (with-current-buffer buf
    (goto-char (point-min))
    (json-parse-buffer)))

(defun knessy--kubectl-get-object-parsed-sync (ns rt name)
"Get one object as JSON synchronously and return it parsed.
NS is the namespace string, RT the resource type string and NAME the
object name.
Run the command from `knessy--kubectl-get-obj-cmd' with :json into a
fresh buffer, then parse it with `knessy--kubectl-parse-json-buffer'.
The buffer is not killed."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name name)))))
    (knessy--shell-exec
     (knessy--kubectl-get-obj-cmd ns rt name :json)
     buf)
    (knessy--kubectl-parse-json-buffer buf)))

;; (defun knessy--kubectl-get-object-sync (name &optional fmt)
;;   (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name name)))))
;;     (knessy--shell-exec
;;      (knessy--kubectl-get-obj-cmd name fmt)
;;      buf)
;;     buf))

;; (defun knessy--kubectl-log-sync ())

;; TODO (pgu, 20.05.2026): this has to be controllable from the transient
(defun knessy--kubectl-log-object-async (ns resource-type name buf buferr follow? tail all-containers? prefix? container timestamps? knessy-prefix? knessy-prefix-width)
"Start streaming the logs of one object into BUF and return the process.
NS, RESOURCE-TYPE and NAME identify the object.
BUF receives the output and BUFERR the standard error.
FOLLOW? makes the command follow the log.
TAIL is a string with the number of lines to start from, or nil.
ALL-CONTAINERS? and CONTAINER choose containers, see
`knessy--kubectl-log-cmd'.
The command always uses --prefix and --timestamps, and its output is
reformatted line by line by `knessy--make-process-filter-logs2'.
PREFIX? tells that filter to print the pod and container prefix, and
TIMESTAMPS? to print the timestamp of each line.
KNESSY-PREFIX-WIDTH is the width the printed prefix is padded to.
KNESSY-PREFIX? is currently unused.
The completion callback does nothing."
  (knessy--shell-exec-async3
    (knessy--kubectl-log-cmd ns resource-type name follow? tail nil nil all-containers? t container t)
    buf
    buferr
    (knessy--make-process-filter-logs2 buf prefix? knessy-prefix-width timestamps?)
    ;; (knessy--make-process-filter-logs buf name knessy-prefix? knessy-prefix-width)
    (lambda ())))

(defun knessy--kubectl-log-labels-async (labels parallel buf buferr follow? tail all-containers? prefix? container timestamps?)
"Start an asynchronous `kubectl logs' for LABELS into BUF.
PARALLEL, BUF, BUFERR, FOLLOW?, TAIL, ALL-CONTAINERS?, PREFIX?,
CONTAINER and TIMESTAMPS? are meant as in `knessy--kubectl-log-cmd'.
Output is passed through `knessy--make-process-filter-logs' with no
name, prefix or width; the completion callback does nothing.
Note: the arguments are passed positionally to
`knessy--kubectl-log-cmd' starting with nil for NS and RESOURCE-TYPE,
which does not line up with that function's parameter order, and this
function is not called anywhere in the project, so it is likely broken."
  (knessy--shell-exec-async3
    (knessy--kubectl-log-cmd nil nil follow? tail parallel labels all-containers? prefix? container timestamps?)
    buf
    buferr
    (knessy--make-process-filter-logs buf nil nil nil)
    (lambda ())))

(defun knessy--kubectl-delete-object-async (ns resource-type name buf buferr force? grace-period now?)
"Start deleting one object in the background and return the process.
NS, RESOURCE-TYPE and NAME identify the object.
FORCE?, GRACE-PERIOD and NOW? are as in
`knessy--kubectl-delete-object-cmd'.
BUF receives the output and BUFERR the standard error.
When the command finishes successfully, the message \"Object(s)
deleted!\" is shown."
  (knessy--shell-exec-async2
    (knessy--kubectl-delete-object-cmd ns resource-type name force? grace-period now?)
    buf
    buferr
    (lambda () (message "Object(s) deleted!"))))

(defun knessy--kubectl-get-object-sync (ns rt name buf &optional fmt)
"Get one object synchronously, writing the output into BUF.
NS is the namespace string, RT the resource type string and NAME the
object name.
BUF is the buffer receiving the output.
FMT is :json, :yaml or nil, as in `knessy--kubectl-get-obj-cmd'.
Return the exit status of the command, as `knessy--shell-exec' does."
  (knessy--shell-exec
   (knessy--kubectl-get-obj-cmd ns rt name fmt)
   buf))

;; TODO (pgu, 20.05.2026): lord why this creates its own buffer but the others get it as a param?
(defun knessy--kubectl-describe-object-sync (ns rt name)
"Describe one object synchronously and return the buffer with the output.
NS is the namespace string, RT the resource type string and NAME the
object name.
The buffer is freshly created, named after NAME, and is not killed."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name name)))))
    (knessy--shell-exec
     (knessy--kubectl-describe-obj-cmd ns rt name)
     buf)
    buf))

(defun knessy--kubectl-apply-file-sync (buf filename)
"Apply the manifest FILENAME synchronously, writing the output into BUF.
BUF is the buffer receiving the output.
Uses the current `knessy--context' and `knessy--namespace'.
Return the exit status of the command, as `knessy--shell-exec' does."
  (knessy--shell-exec
   (knessy--kubectl-apply-file-cmd filename)
   buf))

(defun knessy--kubectl-delete-file-sync (buf filename)
"Delete the objects in FILENAME synchronously, output going to BUF.
BUF is the buffer receiving the output.
Uses the current `knessy--context' and `knessy--namespace'.
The command does not wait for the deletion to complete.
Return the exit status of the command, as `knessy--shell-exec' does."
  (knessy--shell-exec
   (knessy--kubectl-delete-file-cmd filename)
   buf))

(defun knessy--kubectl-validate-file-sync (buf filename)
"Validate the manifest FILENAME with a server-side dry run.
BUF is the buffer receiving the output; standard error is mixed into it.
Uses the current `knessy--context' and `knessy--namespace'.
Nothing is changed in the cluster.
Return the exit status of the command, as `knessy--shell-exec' does."
  (knessy--shell-exec
   (knessy--kubectl-validate-file-cmd filename)
   buf
   t))

(defun knessy--labels-cache-key (ctx ns resource-type)
"Return the key of the labels of RESOURCE-TYPE in NS of CTX.
The key is a list for `knessy--cache':
\(:labels CTX RESOURCE-TYPE) if NS is the all-namespaces marker or
RESOURCE-TYPE is not namespaced, otherwise (:labels CTX NS RESOURCE-TYPE).
All the arguments are explicit, as this is used where the buffer-local
variables are not those of the view that asked for the labels."
  (if (or (knessy--namespace-all? ns)
          (knessy--resource-type-global? resource-type))
      (list :labels ctx resource-type)
    (list :labels ctx ns resource-type)))

(defun knessy--cache-labels-populate-async (ctx ns resource-type)
"Start an asynchronous refresh of the cached labels, unless not needed.
CTX is the context name, NS the namespace and RESOURCE-TYPE the
resource type whose labels are cached, under the key given by
`knessy--labels-cache-key'.
Do nothing if RESOURCE-TYPE is a synthetic *ALL-...* pseudo type or if
the cache entry is still fresh.
Otherwise run the command of `knessy--kubectl-get-labels-cmd' in the
background; note that command is built from the dynamic variables
`knessy--context', `knessy--namespace' and `knessy--resource-type'
at call time, not from the arguments.
When it finishes, the output lines are parsed as JSON (empty lines
count as no labels), merged by
`knessy--utils-ht-merge-duplicates-to-sets', and stored in
`knessy--cache' with `knessy-label-cache-ttl'.
The output buffer is then killed if `knessy-shell-kill-success-buffers'
says so."
  (knessy--log 3 "In knessy--cache-labels-populate-async")
  ;; labels are per-resource-type; there's no single "get labels" call that
  ;; makes sense across every kind at once, and label filtering is refused
  ;; for the *ALL-...* pseudo-types anyway, so skip warming the cache
  (unless (or (knessy--resource-type-all-synthetic? resource-type)
              ;; fetching the labels means downloading every object of the type,
              ;; which is slow in big namespaces: don't repeat it on every refresh
              (knessy--cache-fresh-p knessy--cache (knessy--labels-cache-key ctx ns resource-type)))
    ;; the key is worked out here, where the buffer-local variables are those
    ;; of the view; the callback below runs in whichever buffer is current
    (let ((key (knessy--labels-cache-key ctx ns resource-type))
          (buf (knessy--utils-make-buffer
                (generate-new-buffer-name
                 (knessy--utils-kubectl-buffer-name "labels-cache"))))
          (buferr (knessy--utils-make-buffer
                   (generate-new-buffer-name
                    (knessy--utils-kubectl-buffer-name "labels-cache" nil nil nil t)))))
      ;; FIXME: this is probably bad design -- we always force renew, no matter the ttl
      ;; this should be an aio function that checks the ttl first, and then calls knessy--cache-set
      (knessy--shell-exec-async2
       (knessy--kubectl-get-labels-cmd)
       buf
       buferr
       (lambda ()
         (knessy--log 4 (format "knessy--cache-labels-populate-async: ctx: %s ns: %s resource-type: %s" ctx ns resource-type))
         (knessy--cache-set
          knessy--cache
          key
          (apply
           #'knessy--utils-ht-merge-duplicates-to-sets
           (mapcar
            (lambda (s)
              (if (s-blank? s)
                  (ht)                    ; if there are no labels, the string is an empty string. Just create an empty hashtable instead of parsing
                (json-parse-string s)))
            (knessy--utils-read-buffer buf t)))
          knessy-label-cache-ttl)
         (knessy--kill-success-buffer-maybe buf))))))


;; FIXME: sync label cache renewal steals focus to the buffer with results, same as with main display functions.
;; TODO: cache doesn't work at all for labels?
(comment
 (let ((knessy--context "vam-ttd-p-01")
       (knessy--namespace "spark-dataproc-prod")
       (knessy--resource-type "pods"))
   (knessy--cache-labels-populate-async))

 (let ((knessy--context "vam-ttd-p-01")
       (knessy--namespace "monitoring")
       (knessy--resource-type "pods")
       (buf (get-buffer "*knessy-vam-ttd-p-01-monitoring-pods-labels-cache*<40>")))
   (lambda ()
       (knessy--cache-set
        knessy--cache
        ;; TODO: maybe (knessy--namespace-all?) should take no arguments
        (if (or (knessy--namespace-all? knessy--namespace)
                (knessy--resource-type-global?))
            (list :labels knessy--context knessy--resource-type)
          (list :labels knessy--context knessy--namespace knessy--resource-type))
        (apply
         #'knessy--utils-ht-merge-duplicates-to-sets
         (mapcar
          (lambda (s) (json-parse-string s))
          (knessy--utils-read-buffer buf t)))
        knessy-label-cache-ttl)))

 (princ (ht-get (ht-get (ht-get knessy--cache :labels) "minikube") "pods"))
 (princ (ht-get (ht-get (ht-get (ht-get knessy--cache :labels) "vam-ttd-p-01") "monitoring") "pods")))


;; TODO: kubectl forming commands already becoming dirty, generalize
;; TODO: also, resolve the kubeconfig env problem

;; TODO: what to do with clusters we couldn't log in to? like euid? (maybe redirect errors to a separate buffer with make-process)

(defun knessy-caches-refresh ()
"Rebuild all the main caches.
Clear `knessy--cache' with `knessy-cache-clear' and then start
`knessy--caches-populate-async', which refreshes contexts, namespaces
and api-resources in the background."
  (interactive)
  (knessy-cache-clear)
  (knessy--caches-populate-async))

(comment
 knessy--cache
 (knessy--caches-populate-async)
 (setenv "KUBECONFIG" "/home/pgu/.kube/config:/home/pgu/.kube/k8s-local")
 (ht-get knessy--cache-namespaces "wa1-aks-p-01")

 (-> knessy--cache
     (ht-get :resource-types-global)
     (ht-keys)))


(provide 'knessy-kubectl)
