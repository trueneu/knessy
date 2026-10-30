;; -*- lexical-binding: t; -*-

;; The *LINKED* view: given one resource, list everything that is linked
;; to it by ownership or by references.
;;
;; dependencies: cl-lib, dash, s, ht, asoc
;;
;; How it works: the whole namespace of the selected resource is fetched with a
;; single `kubectl get <all listable types> -o json' call, and the graph is
;; walked in memory. Only cluster-scoped objects (nodes, PVs, ...) are fetched
;; one by one, on demand.
;;
;; The walk is deliberately directional. Following every edge both ways would
;; make a pod "linked" to all the pods of its service, and through its node
;; to the whole cluster. Starting at the selected resource ("root"):
;;
;; 1. ancestors:   owners of the root, owners of the owners, ...
;; 2. descendants: objects owned by the root, and those owned by them, ...
;;    (the root, its ancestors and its descendants together are the "core")
;; 3. forward:     things the core refers to (configmaps, secrets, PVCs, the
;;    service account, the node...), and things *those* refer to (PVC -> PV).
;;    Only forward edges are followed from there on.
;; 4. incoming:    things that refer to the core (a service selecting a pod, an
;;    HPA targeting a deployment...), and things that refer to *those*
;;    (an ingress or an HTTPRoute pointing to the service). Only incoming
;;    edges are followed from there on, except that the gateways of the
;;    routes found are included too, along with what they refer to (TLS
;;    certificates), but not what else is attached to them.

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)
(require 'asoc)

(defvar-local knessy--linked-root nil
  "The (NS RT NAME) of the resource the *LINKED* view is built around.
NS is nil if the resource is cluster-scoped.")

(defcustom knessy-linked-ignored-resource-types
  '("events" "events.events.k8s.io" "pods.metrics.k8s.io")
  "Resource types that the *LINKED* view never fetches or shows.
Events are left out because they are noisy and can be huge; there's an
event for nearly everything that happens to a resource."
  :type '(repeat string)
  :group 'knessy)

(defconst knessy--linked-cluster-scoped-kinds
  '("Node" "PersistentVolume" "StorageClass" "PriorityClass" "ClusterSecretStore")
  "Kinds that references in specs may point to, which have no namespace.")

;; ---------------------------------------------------------------------------
;; Parsed JSON helpers
;; ---------------------------------------------------------------------------

(defun knessy--linked-get (obj &rest keys)
  "Look KEYS up in the nested hash tables of OBJ.
OBJ is a parsed JSON object (a hash table).
KEYS are the successive keys to look up, one per nesting level.
Return the value found, or nil if any step is missing or is not a hash table."
  (let ((current obj))
    (while (and keys current)
      (setq current (when (hash-table-p current)
                      (gethash (pop keys) current))))
    current))

(defun knessy--linked-kind (obj)
  "Return the \"kind\" of the parsed Kubernetes object OBJ, or nil."
  (knessy--linked-get obj "kind"))
(defun knessy--linked-name (obj)
  "Return the metadata name of the parsed Kubernetes object OBJ, or nil."
  (knessy--linked-get obj "metadata" "name"))
(defun knessy--linked-ns (obj)
  "Return the metadata namespace of the parsed object OBJ.
The result is nil for cluster-scoped objects."
  (knessy--linked-get obj "metadata" "namespace"))

(defun knessy--linked-key (obj)
  "Return the (KIND NS NAME) triple of the parsed object OBJ.
This is the form in which references from other objects name it.
NS is nil for cluster-scoped objects."
  (list (knessy--linked-kind obj)
        (knessy--linked-ns obj)
        (knessy--linked-name obj)))

(defun knessy--linked-uid (obj)
  "Return a unique identifier of the parsed object OBJ.
This is its metadata uid, or, if it has none, the printed form of its
\(KIND NS NAME) key as a string."
  (or (knessy--linked-get obj "metadata" "uid")
      (format "%S" (knessy--linked-key obj))))

(defun knessy--linked-walk (node fn)
  "Call FN with (KEY VALUE) for every pair of every hash table nested in NODE.
NODE is a parsed JSON value: hash tables and vectors are descended into
recursively, other values are ignored.
FN is called for its side effects, and its return value is discarded.
Return nil."
  (cond ((hash-table-p node)
         (maphash (lambda (k v)
                    (funcall fn k v)
                    (knessy--linked-walk v fn))
                  node))
        ((vectorp node)
         (mapc (lambda (e) (knessy--linked-walk e fn)) node))))

;; ---------------------------------------------------------------------------
;; The index
;; ---------------------------------------------------------------------------

(cl-defstruct (knessy--linked-index
               (:conc-name knessy--ci-)
               (:constructor knessy--ci-make)
               (:copier nil))
  (by-uid (ht))
  (by-key (ht))
  (owned-by (ht))                       ; owner uid -> objects it owns
  (referrers (ht))                      ; (KIND NS NAME) -> objects referring to it
  (pods nil)
  (dumped-ns nil)                       ; the namespace that all the objects were listed from
  (requested (ht))                      ; (KIND NS NAME) of objects that were looked up in vain
  (pending nil))                        ; the ones of those that are worth fetching

(defun knessy--ci-add (idx obj)
  "Register the parsed object OBJ in the index IDX.
OBJ is looked up by its uid, and is added to the by-uid and by-key tables,
to the pod list if it is a Pod, and to the owned-by table under each of its
owner references.
Return the registered object, which is the one already in IDX if OBJ was
known by uid, and OBJ otherwise."
  (let ((uid (knessy--linked-uid obj)))
    (or (ht-get (knessy--ci-by-uid idx) uid)
        (progn
          (ht-set (knessy--ci-by-uid idx) uid obj)
          (ht-set (knessy--ci-by-key idx) (knessy--linked-key obj) obj)
          (when (equal (knessy--linked-kind obj) "Pod")
            (push obj (knessy--ci-pods idx)))
          (seq-doseq (ref (knessy--linked-get obj "metadata" "ownerReferences"))
            (let ((owner-uid (knessy--linked-get ref "uid")))
              (ht-set (knessy--ci-owned-by idx) owner-uid
                      (cons obj (ht-get (knessy--ci-owned-by idx) owner-uid)))))
          obj))))

(defun knessy--ci-request (idx kind ns name)
  "Note that the object (KIND NS NAME) is missing from IDX and should be fetched.
IDX is the index.
KIND and NS and NAME identify the object; NS is nil if it is cluster-scoped.
Only cluster-scoped objects and objects of namespaces other than the dumped one
are worth fetching: anything else that's not in IDX doesn't exist (e.g. an
optional configmap).
Each object is requested at most once; requests are pushed onto the pending
list of IDX.
Do nothing if KIND or NAME is nil.
Return value is not meaningful."
  (let ((key (list kind ns name)))
    (when (and kind name
               (or (null ns) (not (equal ns (knessy--ci-dumped-ns idx))))
               (not (ht-contains? (knessy--ci-requested idx) key)))
      (ht-set (knessy--ci-requested idx) key t)
      (push key (knessy--ci-pending idx)))))

(defun knessy--ci-lookup (idx kind ns name)
  "Return the object (KIND NS NAME) if the index IDX has it.
If not, return nil, and request the object (see `knessy--ci-request'), so that
it's there after the next round of fetching (see `knessy--linked-ids-aio')."
  (or (ht-get (knessy--ci-by-key idx) (list kind ns name))
      (progn (knessy--ci-request idx kind ns name)
             nil)))

;; ---------------------------------------------------------------------------
;; Edges
;; ---------------------------------------------------------------------------

(defun knessy--linked-selector (obj)
  "Return the label selector of OBJ if it selects pods, else nil.
OBJ is a parsed object.
The selector is a parsed matchLabels/matchExpressions table.
A Service gets one made of its spec selector, unless that is empty.
Only the kinds for which pods aren't already reachable through ownership
are considered, otherwise a pod would be linked to all its siblings."
  (pcase (knessy--linked-kind obj)
    ("Service" (let ((selector (knessy--linked-get obj "spec" "selector")))
                 ;; a service without a selector selects nothing
                 (when (and selector (> (hash-table-count selector) 0))
                   (ht ("matchLabels" selector)))))
    ("PodDisruptionBudget" (knessy--linked-get obj "spec" "selector"))
    ("NetworkPolicy" (knessy--linked-get obj "spec" "podSelector"))))

(defun knessy--linked-selector-matches? (selector labels)
  "Return non-nil if the label selector SELECTOR matches LABELS.
SELECTOR is a parsed table with optional matchLabels and matchExpressions.
The operators In, NotIn, Exists and DoesNotExist are supported; an
expression with any other operator never matches.
LABELS is a hash table of labels, or nil, which is the same as no labels."
  (let ((labels (or labels (ht)))
        (match-labels (knessy--linked-get selector "matchLabels"))
        (match-expressions (knessy--linked-get selector "matchExpressions")))
    (and (cl-every (lambda (k) (equal (ht-get match-labels k) (ht-get labels k)))
                   (when match-labels (ht-keys match-labels)))
         (cl-every
          (lambda (expr)
            (let* ((k (knessy--linked-get expr "key"))
                   (values (append (knessy--linked-get expr "values") nil))
                   (present? (ht-contains? labels k))
                   (v (ht-get labels k)))
              (pcase (knessy--linked-get expr "operator")
                ("In" (and present? (member v values)))
                ("NotIn" (or (not present?) (not (member v values))))
                ("Exists" present?)
                ("DoesNotExist" (not present?))
                (_ nil))))
          (append match-expressions nil)))))

(defun knessy--linked-forward-targets (idx obj)
  "Return the list of (KIND NS NAME) triples that OBJ refers to.
IDX is the index; its pod list is used to resolve the pods that a selecting
object (see `knessy--linked-selector') matches in the namespace of OBJ.
OBJ is a parsed object.
References are found structurally, wherever they appear in the object, so that
pod templates of any workload kind (including CRDs) are covered.
Cluster-scoped kinds get a nil NS.
The result has no duplicates.
This does not fetch or request anything."
  (let* ((kind (knessy--linked-kind obj))
         (ns (knessy--linked-ns obj))
         (targets '()))
    (cl-flet ((add (target-kind target-ns target-name)
                (when (and (stringp target-kind) (stringp target-name))
                  (push (list target-kind
                              (unless (member target-kind knessy--linked-cluster-scoped-kinds)
                                target-ns)
                              target-name)
                        targets))))
      (let ((visit
             (lambda (k v)
               (cond
                ((stringp v)
                 (pcase k
                   ("serviceAccountName" (add "ServiceAccount" ns v))
                   ("priorityClassName" (add "PriorityClass" nil v))
                   ("storageClassName" (add "StorageClass" nil v))))
                ((hash-table-p v)
                 (pcase k
                   ((or "configMapKeyRef" "configMapRef" "configMap")
                    (add "ConfigMap" ns (knessy--linked-get v "name")))
                   ((or "secretKeyRef" "secretRef")
                    (add "Secret" ns (knessy--linked-get v "name")))
                   ("secret"
                    (add "Secret" ns (or (knessy--linked-get v "secretName")
                                         (knessy--linked-get v "name"))))
                   ("persistentVolumeClaim"
                    (add "PersistentVolumeClaim" ns (knessy--linked-get v "claimName")))
                   ;; e.g. ingress backends
                   ("service"
                    (add "Service" (or (knessy--linked-get v "namespace") ns)
                         (knessy--linked-get v "name")))
                   ;; external secret -> its (cluster) secret store
                   ("secretStoreRef"
                    (add (or (knessy--linked-get v "kind") "SecretStore") ns
                         (knessy--linked-get v "name")))
                   ;; endpoints -> pod, hpa -> workload, pv -> pvc
                   ((or "targetRef" "scaleTargetRef" "claimRef")
                    (add (knessy--linked-get v "kind")
                         (or (knessy--linked-get v "namespace") ns)
                         (knessy--linked-get v "name")))))
                ((and (vectorp v) (equal k "imagePullSecrets"))
                 (seq-doseq (e v)
                   (add "Secret" ns (knessy--linked-get e "name"))))
                ;; gateway API routes: what they send traffic to, and the
                ;; gateways they are attached to
                ;; gateway listeners: the TLS certificates (secrets by default)
                ((and (vectorp v) (equal k "certificateRefs"))
                 (seq-doseq (e v)
                   (add (or (knessy--linked-get e "kind") "Secret")
                        (or (knessy--linked-get e "namespace") ns)
                        (knessy--linked-get e "name"))))
                ((and (vectorp v) (member k '("backendRefs" "parentRefs")))
                 (seq-doseq (e v)
                   (add (or (knessy--linked-get e "kind")
                            (if (equal k "backendRefs") "Service" "Gateway"))
                        (or (knessy--linked-get e "namespace") ns)
                        (knessy--linked-get e "name"))))))))
        (maphash (lambda (k v)
                   ;; metadata is handled separately (owners); the data of
                   ;; configmaps and secrets is user content, not references
                   (unless (member k '("metadata" "kind" "apiVersion" "data" "binaryData" "stringData"))
                     (funcall visit k v)
                     (knessy--linked-walk v visit)))
                 obj))
      (pcase kind
        ;; the Endpoints of a service are tied to it by name only
        ("Service" (add "Endpoints" ns (knessy--linked-name obj)))
        ("Pod" (add "Node" nil (knessy--linked-get obj "spec" "nodeName")))
        ("PersistentVolumeClaim" (add "PersistentVolume" nil (knessy--linked-get obj "spec" "volumeName"))))
      (when-let* ((selector (knessy--linked-selector obj)))
        (dolist (pod (knessy--ci-pods idx))
          (when (and (equal (knessy--linked-ns pod) ns)
                     (knessy--linked-selector-matches?
                      selector (knessy--linked-get pod "metadata" "labels")))
            (add "Pod" ns (knessy--linked-name pod))))))
    (delete-dups targets)))

(defun knessy--linked-index-referrers (idx)
  "Fill the reverse references table of IDX from the objects registered so far.
For each object, each of its forward targets (see
`knessy--linked-forward-targets') gets the object added as a referrer.
Return value is not meaningful."
  (let ((referrers (knessy--ci-referrers idx)))
    (dolist (obj (ht-values (knessy--ci-by-uid idx)))
      (dolist (target (knessy--linked-forward-targets idx obj))
        (ht-set referrers target (cons obj (ht-get referrers target)))))))

(defun knessy--linked-owners (idx obj)
  "Return the list of objects that own OBJ according to its ownerReferences.
IDX is the index.
Each owner is looked up by uid first, then by kind and the namespace of OBJ,
then by kind as a cluster-scoped object.
Owners that are not found are left out, and are requested for the next
fetching round (see `knessy--ci-lookup')."
  (let ((ns (knessy--linked-ns obj)))
    (delq nil
          (mapcar (lambda (ref)
                    (let ((kind (knessy--linked-get ref "kind"))
                          (name (knessy--linked-get ref "name")))
                      (or (ht-get (knessy--ci-by-uid idx) (knessy--linked-get ref "uid"))
                          (knessy--ci-lookup idx kind ns name)
                          ;; the owner of a namespaced object may be cluster-scoped
                          (knessy--ci-lookup idx kind nil name))))
                  (append (knessy--linked-get obj "metadata" "ownerReferences") nil)))))

(defun knessy--linked-owned (idx obj)
  "Return the list of objects in IDX that are owned by OBJ, or nil."
  (ht-get (knessy--ci-owned-by idx) (knessy--linked-uid obj)))

(defun knessy--linked-forward (idx obj)
  "Return the objects that OBJ refers to, and, for secrets, their owners.
IDX is the index.
Targets that are not in IDX are left out, and are requested for the next
fetching round (see `knessy--ci-lookup').
Secrets are often generated from something (an ExternalSecret, a
cert-manager Certificate), which is worth showing. For other kinds the owners
of referenced objects are left out: that'd drag in unrelated things, like the
StatefulSet that owns a PVC."
  (delq nil
        (append
         (mapcar (lambda (target) (apply #'knessy--ci-lookup idx target))
                 (knessy--linked-forward-targets idx obj))
         (when (equal (knessy--linked-kind obj) "Secret")
           (knessy--linked-owners idx obj)))))

(defun knessy--linked-incoming (idx obj)
  "Return the list of objects in IDX that refer to OBJ, or nil.
The reverse references table must have been filled before, see
`knessy--linked-index-referrers'."
  (ht-get (knessy--ci-referrers idx) (knessy--linked-key obj)))

(defun knessy--linked-gateways (idx obj)
  "Return the gateways that OBJ (a route) is attached to.
IDX is the index.
Gateways that are not in IDX are left out, and are requested for the next
fetching round (see `knessy--ci-lookup')."
  (delq nil
        (mapcar (lambda (target) (apply #'knessy--ci-lookup idx target))
                (-filter (lambda (target) (equal (car target) "Gateway"))
                         (knessy--linked-forward-targets idx obj)))))

;; ---------------------------------------------------------------------------
;; The walk
;; ---------------------------------------------------------------------------

(defun knessy--linked-closure (starts next &optional exclude)
  "Return the objects reachable from STARTS by repeatedly applying NEXT.
The search is breadth-first, so the result is ordered nearest first.
NEXT is a function that takes an object and returns a list of objects.
STARTS is a list of objects to start from.
EXCLUDE is an optional list of objects that are not entered.
Neither STARTS nor EXCLUDE are part of the result."
  (let ((seen (ht))
        (queue (copy-sequence starts))
        (found '()))
    (dolist (obj (append starts exclude))
      (ht-set seen (knessy--linked-uid obj) t))
    (while queue
      (dolist (obj (funcall next (pop queue)))
        (unless (ht-contains? seen (knessy--linked-uid obj))
          (ht-set seen (knessy--linked-uid obj) t)
          (push obj found)
          (setq queue (append queue (list obj))))))
    (nreverse found)))

(defun knessy--linked-graph (idx root)
  "Return all objects linked to ROOT (see the commentary on top of the file).
IDX is the index, and ROOT is the parsed object the walk starts at.
Objects missing from IDX that the walk would need are requested, not fetched
\(see `knessy--ci-request').
The result is ordered: ancestors (top-most first), ROOT, descendants,
forward references, incoming references, gateways of incoming routes,
what those gateways refer to."
  (let* ((ancestors (knessy--linked-closure
                     (list root) (lambda (o) (knessy--linked-owners idx o))))
         (descendants (knessy--linked-closure
                       (list root) (lambda (o) (knessy--linked-owned idx o))
                       ancestors))
         (core (append (list root) ancestors descendants))
         (forward (knessy--linked-closure
                   core (lambda (o) (knessy--linked-forward idx o))))
         (incoming (knessy--linked-closure
                    core (lambda (o) (knessy--linked-incoming idx o))
                    forward))
         ;; the gateways of the incoming routes; their own incoming edges
         ;; (all the other routes attached to them) are not followed
         (gateways (knessy--linked-closure
                    incoming (lambda (o) (knessy--linked-gateways idx o))
                    (append core forward)))
         ;; what the gateways refer to (TLS certificates)
         (gateway-refs (knessy--linked-closure
                        gateways (lambda (o) (knessy--linked-forward idx o))
                        (append core forward incoming))))
    (append (reverse ancestors) (list root) descendants forward incoming
            gateways gateway-refs)))

;; ---------------------------------------------------------------------------
;; kubectl glue
;; ---------------------------------------------------------------------------

(defun knessy--linked-kubectl-json-aio (context args &optional tolerant?)
  "Run `kubectl ARGS -o json' in CONTEXT asynchronously.
CONTEXT is the kubectl context name.
ARGS is a string of kubectl arguments, inserted into a shell command as is.
TOLERANT? is optional; if non-nil, empty output yields nil instead of an error.
Returns a promise of the parsed output. If there's no output at all, awaiting it
signals an error with what kubectl said, or yields nil if TOLERANT?. Partial
output is accepted, because kubectl exits non-zero if it can't list just one of
many types.
Side effects: starts a bash process that writes to a temporary file, which is
deleted when the process ends; uses `knessy-kubectl' and `knessy-kubeconfig'.

CONTEXT is passed explicitly: buffer-local variables can't be relied on in
asynchronous code, as the user may switch buffers any time."
  (let* ((promise (aio-promise))
         (buf (generate-new-buffer " *knessy-linked*" t))
         (buferr (generate-new-buffer " *knessy-linked-stderr*" t))
         ;; kubectl writes to a file rather than to Emacs: it writes in small
         ;; pieces, and Emacs wakes up for each of them, which makes receiving
         ;; tens of MB (the pods of a big namespace) take about a minute in a
         ;; busy Emacs session. Reading a finished file takes a fraction of a second.
         (outfile (make-temp-file "knessy-linked-"))
         (cmd (s-concat knessy-kubectl " --context " context " " args " -o json > "
                        (shell-quote-argument outfile))))
    (knessy--log 3 cmd)
    (with-environment-variables (("KUBECONFIG" knessy-kubeconfig))
      (make-process
       :name "knessy-linked"
       :command (list "/bin/bash" "-c" cmd)
       :buffer nil
       :stderr buferr
       :noquery t
       :sentinel (lambda (proc _event)
                   (unless (process-live-p proc)
                     (let (value failure)
                       (condition-case err
                           (with-current-buffer buf
                             (let ((coding-system-for-read 'utf-8-unix))
                               (insert-file-contents outfile))
                             (cond ((> (buffer-size) 0)
                                    (goto-char (point-min))
                                    (setq value (json-parse-buffer :null-object nil :false-object nil)))
                                   (tolerant? nil)
                                   (t (error "kubectl %s failed: %s" args
                                             (s-trim (with-current-buffer buferr (buffer-string)))))))
                         (error (setq failure err)))
                       (ignore-errors (delete-file outfile))
                       (kill-buffer buf)
                       (kill-buffer buferr)
                       (aio-resolve promise
                                    (lambda ()
                                      (if failure
                                          (signal (car failure) (cdr failure))
                                        value))))))))
    promise))

(defun knessy--linked-api-resources ()
  "Return the API resources of the current context, with :kind and :group.
Caches populated by older versions of Knessy don't have them; if the cached
list lacks :kind, it is replaced by a freshly fetched one (a kubectl call,
and a write to `knessy--cache').
The result is a list of alists, as returned by `knessy--api-resources'."
  (let ((resources (knessy--api-resources)))
    (if (and resources (not (asoc-get (car resources) :kind)))
        (knessy--cache-set knessy--cache
                           (list :api-resources knessy--context)
                           (knessy--kubectl-api-resources-list))
      resources)))

(defun knessy--linked-find-resource (resources type)
  "Return the API resource in RESOURCES whose name or kind is TYPE, or nil.
RESOURCES is a list of API resource alists.
TYPE is a resource type name or a kind; the name is matched first, then the
kind, then the kind ignoring case.
Types in `knessy-linked-ignored-resource-types' are never returned."
  (let ((candidates (-remove (lambda (r) (member (asoc-get r :name) knessy-linked-ignored-resource-types))
                             resources)))
    (or (-find (lambda (r) (equal (asoc-get r :name) type)) candidates)
        (-find (lambda (r) (equal (asoc-get r :kind) type)) candidates)
        (-find (lambda (r) (s-equals? (downcase (asoc-get r :kind)) (downcase type))) candidates))))

(defun knessy--linked-resource-type-of (resources obj)
  "Return the fully-qualified resource type name (as in `kubectl api-resources').
RESOURCES is a list of API resource alists.
OBJ is a parsed object; its kind and the group of its apiVersion are used to
pick the resource, falling back to the first resource of that kind, and then
to the lowercased kind.
Types in `knessy-linked-ignored-resource-types' are not considered."
  (let* ((kind (knessy--linked-kind obj))
         (api-version (or (knessy--linked-get obj "apiVersion") ""))
         (group (if (s-contains? "/" api-version) (car (s-split "/" api-version)) ""))
         (candidates (-filter (lambda (r) (and (equal (asoc-get r :kind) kind)
                                               (not (member (asoc-get r :name)
                                                            knessy-linked-ignored-resource-types))))
                              resources)))
    (asoc-get (or (-find (lambda (r) (equal (asoc-get r :group) group)) candidates)
                  (car candidates)
                  `((:name . ,(downcase kind))))
              :name)))

(defcustom knessy-linked-parallelism 8
  "How many kubectl calls the *LINKED* view makes at the same time."
  :type 'integer
  :group 'knessy)

(defcustom knessy-linked-types-per-call 10
  "How many resource types the *LINKED* view lists in a single kubectl call.
Every kubectl call has a fixed cost of about a second (startup, authentication,
discovery), so listing the types one by one is slow, and listing all of them in
one call is slow too, as kubectl does it sequentially."
  :type 'integer
  :group 'knessy)

(defcustom knessy-linked-heavy-resource-types '("pods")
  "Resource types that the *LINKED* view always lists in a call of their own.
Their output is big, so they shouldn't hold up the types they'd be grouped with."
  :type '(repeat string)
  :group 'knessy)

(defun knessy--linked-chunk-types (types)
  "Split TYPES into groups of at most `knessy-linked-types-per-call' types.
TYPES is a list of resource type names.
Return a list of comma-separated strings.
Types of `knessy-linked-heavy-resource-types' get a group of their own.
The order of TYPES is kept. Each group is a comma-separated string, as
`kubectl get' takes it."
  (let ((chunks '()) (current '()))
    (cl-flet ((flush () (when current
                          (push (s-join "," (nreverse current)) chunks)
                          (setq current '()))))
      (dolist (type types)
        (if (member type knessy-linked-heavy-resource-types)
            (progn (flush) (push type chunks))
          (push type current)
          (when (>= (length current) knessy-linked-types-per-call)
            (flush))))
      (flush))
    (nreverse chunks)))

(aio-defun knessy--linked-fetch-types-worker (context ns queue results)
  "Fetch the objects of the groups of types in QUEUE in namespace NS.
CONTEXT is the kubectl context.
RESULTS is the vector described below.
This is an asynchronous function that returns a promise; its value is not
meaningful.
Groups that fail to be listed are skipped.
QUEUE is a cons cell shared between the workers, holding a list of
(INDEX . TYPES) in its car; a worker takes groups off it until there are none
left. The objects of the group number INDEX are stored at that index of the
vector RESULTS."
  (let (item)
    (while (setq item (pop (car queue)))
      (let ((json (condition-case nil
                      ;; tolerant: types that can't be listed (e.g. forbidden) are skipped
                      (aio-await (knessy--linked-kubectl-json-aio
                                  context
                                  (format "-n %s get %s" (shell-quote-argument ns) (cdr item))
                                  t))
                    (error nil))))
        (when json
          (aset results (car item)
                (append (knessy--linked-get json "items") nil)))))))

(aio-defun knessy--linked-fetch-namespace-aio (context ns resources)
  "Return a promise of the list of all objects of all listable types in NS.
CONTEXT is the kubectl context.
NS is the namespace.
RESOURCES are the API resources of CONTEXT; the namespaced ones that support
the list verb, except `knessy-linked-ignored-resource-types', are listed.
The types are listed in groups (see `knessy--linked-chunk-types'),
`knessy-linked-parallelism' kubectl calls at a time. The objects come back in
the order of the types, whichever call finishes first."
  (let* ((types (->> resources
                     (-filter (lambda (r) (and (asoc-get r :namespaced)
                                               (member "list" (asoc-get r :verbs))
                                               (not (member (asoc-get r :name)
                                                            knessy-linked-ignored-resource-types)))))
                     (mapcar (lambda (r) (asoc-get r :name)))))
         (chunks (knessy--linked-chunk-types types))
         (results (make-vector (length chunks) nil))
         (queue (list (-map-indexed #'cons chunks)))
         (workers (cl-loop repeat (max 1 (min knessy-linked-parallelism (length chunks)))
                           collect (knessy--linked-fetch-types-worker context ns queue results))))
    (dolist (worker workers)
      (aio-await worker))
    (apply #'append (append results nil))))

(defun knessy--linked-fetch-object-aio (context resources kind ns name)
  "Return a promise of the object (KIND NS NAME), or of nil if it can't be fetched.
CONTEXT is the kubectl context.
RESOURCES are the API resources of CONTEXT, used to find the resource type
of KIND.
NS is nil for cluster-scoped objects.
Runs `kubectl get' asynchronously, unless KIND is unknown, in which case the
promise is already resolved with nil."
  (if-let* ((resource (knessy--linked-find-resource resources kind)))
      (knessy--linked-kubectl-json-aio
       context
       (format "%s get %s/%s"
               (if ns (format "-n %s" (shell-quote-argument ns)) "")
               (asoc-get resource :name)
               (shell-quote-argument name))
       t)
    (let ((promise (aio-promise)))
      (aio-resolve promise (lambda () nil))
      promise)))

(aio-defun knessy--linked-ids-aio (context resources ns rt name)
  "IDs of all the resources linked to resource NAME of type RT in namespace NS.
CONTEXT is the kubectl context.
NS is nil for cluster-scoped resources. The IDs are as in the other Knessy views,
i.e. (NS . (RT . NAME)). RESOURCES are the API resources of CONTEXT.
Return a promise of the list of IDs, ordered as in `knessy--linked-graph'.
Signals a `user-error' if RT is unknown, and an error if the root resource
can't be fetched.

All the kubectl calls are asynchronous. The graph walk itself is not, so it
can't fetch objects it misses (cluster-scoped ones, gateways of other
namespaces...) by itself. Instead it notes them (see `knessy--ci-request'),
and when it's done they're all fetched in parallel, and the walk is repeated.
It takes as many rounds as the longest chain of such references."
  (let* ((root-resource (or (knessy--linked-find-resource resources rt)
                            (user-error "Unknown resource type %s" rt)))
         (idx (knessy--ci-make :dumped-ns ns)))
    (when ns
      (dolist (obj (aio-await (knessy--linked-fetch-namespace-aio context ns resources)))
        (knessy--ci-add idx obj))
      (knessy--linked-index-referrers idx))
    (unless (ht-get (knessy--ci-by-key idx) (list (asoc-get root-resource :kind) ns name))
      (let ((root-obj (aio-await
                       (knessy--linked-fetch-object-aio
                        context resources (asoc-get root-resource :kind) ns name))))
        (unless root-obj
          (error "Could not fetch %s/%s" rt name))
        (knessy--ci-add idx root-obj)))
    (let* ((root (ht-get (knessy--ci-by-key idx) (list (asoc-get root-resource :kind) ns name)))
           (graph (knessy--linked-graph idx root)))
      (while (knessy--ci-pending idx)
        (let ((promises (mapcar (lambda (key)
                                  (apply #'knessy--linked-fetch-object-aio context resources key))
                                (knessy--ci-pending idx))))
          (setf (knessy--ci-pending idx) nil)
          (dolist (promise promises)
            (let ((obj (aio-await promise)))
              (when obj
                (knessy--ci-add idx obj)))))
        (setq graph (knessy--linked-graph idx root)))
      (mapcar (lambda (obj)
                (cons (knessy--linked-ns obj)
                      (cons (knessy--linked-resource-type-of resources obj)
                            (knessy--linked-name obj))))
              graph))))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defun knessy--linked-show (display-buf ids)
  "Display the resources with IDS in DISPLAY-BUF.
IDS is a list of IDs, (NS . (RT . NAME)), in display order.
DISPLAY-BUF is the Knessy buffer, whose current view is used.
Each resource is shown with RESOURCE-TYPE, NAMESPACE and NAME columns.
Return value is not meaningful."
  (with-current-buffer display-buf
    (let ((items (ht))
          (view (ht-get knessy-views (cons knessy--resource-type knessy--view))))
      ;; `ht-items' hands the items back in reverse order of insertion
      (dolist (id (reverse ids))
        (ht-set items id (ht ("RESOURCE-TYPE" (knessy--id-rt id))
                             ("NAMESPACE" (or (knessy--id-ns id) ""))
                             ("NAME" (knessy--id-name id)))))
      (knessy--display2-common-part
       display-buf
       view
       `(((:items . ,items)
          (:headers . ((:static . ("RESOURCE-TYPE" "NAMESPACE" "NAME"))))))
       nil))))

(defun knessy--linked-still-current? (display-buf env)
  "Return non-nil if DISPLAY-BUF is alive and still has the env ENV.
ENV is the env (see `knessy--env') that was current when the linked
resources were requested."
  (and (buffer-live-p display-buf)
       (with-current-buffer display-buf
         (equal (knessy--env) env))))

(aio-defun knessy--display-linked-aio ()
  "Fetch and display the resources linked to `knessy--linked-root'.
Work on the current buffer, which must be a Knessy buffer.
Do nothing if `knessy--refresh-in-progress' is set; otherwise set it, unless
`knessy--debug' is non-nil.
The buffer state is captured before the first await, and the result is
dropped if the env of the buffer changed meanwhile.
On success `knessy--linked-show' paints the buffer, and the flag is not reset
by this function in that case.
On failure or when the result is dropped, the flag is reset here, and on
failure a message is shown if the buffer is still current.
This is an asynchronous function that returns a promise."
  (unless knessy--refresh-in-progress
    (if knessy--debug
        (setq knessy--refresh-in-progress nil)
      (setq knessy--refresh-in-progress t))
    ;; everything the code below needs from the buffer-local state is grabbed
    ;; here, before the first await
    (let ((display-buf (current-buffer))
          (env (knessy--env))
          (context knessy--context)
          (root knessy--linked-root)
          (resources (knessy--linked-api-resources)))
      (condition-case err
          (let ((ids (aio-await (apply #'knessy--linked-ids-aio context resources root))))
            ;; this can take long (tens of seconds in big namespaces), and the
            ;; user may have gone elsewhere meanwhile: don't paint over that
            (if (knessy--linked-still-current? display-buf env)
                (knessy--linked-show display-buf ids)
              (knessy--log 3 "Linked resources arrived after the view changed, dropping them")
              (when (buffer-live-p display-buf)
                (with-current-buffer display-buf
                  (setq knessy--refresh-in-progress nil)))))
        (error
         (when (buffer-live-p display-buf)
           (with-current-buffer display-buf
             (setq knessy--refresh-in-progress nil)))
         (when (knessy--linked-still-current? display-buf env)
           (message "Could not get the linked resources: %s" (error-message-string err))))))))

(defun knessy--display-linked (&optional sync)
  "Display the resources linked to `knessy--linked-root' in the current buffer.
Asynchronously, unless SYNC is non-nil, in which case wait until done.
Signals a `user-error' if `knessy--linked-root' is nil."
  (unless knessy--linked-root
    (user-error "No resource to show the linked resources of; use the jump command on a resource"))
  (if sync
      (aio-wait-for (knessy--display-linked-aio))
    (knessy--display-linked-aio)))

(provide 'knessy-linked)
