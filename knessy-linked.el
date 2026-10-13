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
;;    (an ingress pointing to the service). Only incoming edges are followed
;;    from there on.

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
  '("Node" "PersistentVolume" "StorageClass" "PriorityClass")
  "Kinds that references in specs may point to, which have no namespace.")

;; ---------------------------------------------------------------------------
;; Parsed JSON helpers
;; ---------------------------------------------------------------------------

(defun knessy--linked-get (obj &rest keys)
  "Look KEYS up in the nested hash tables of OBJ; nil if any step is missing."
  (let ((current obj))
    (while (and keys current)
      (setq current (when (hash-table-p current)
                      (gethash (pop keys) current))))
    current))

(defun knessy--linked-kind (obj) (knessy--linked-get obj "kind"))
(defun knessy--linked-name (obj) (knessy--linked-get obj "metadata" "name"))
(defun knessy--linked-ns (obj) (knessy--linked-get obj "metadata" "namespace"))

(defun knessy--linked-key (obj)
  "The (KIND NS NAME) triple that references from other objects use."
  (list (knessy--linked-kind obj)
        (knessy--linked-ns obj)
        (knessy--linked-name obj)))

(defun knessy--linked-uid (obj)
  (or (knessy--linked-get obj "metadata" "uid")
      (format "%S" (knessy--linked-key obj))))

(defun knessy--linked-walk (node fn)
  "Call FN with (KEY VALUE) for every pair of every hash table nested in NODE."
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
  (resolver nil))                       ; (KIND NS NAME) -> object, fetched on demand

(defun knessy--ci-add (idx obj)
  "Register OBJ in IDX. Returns the registered object (OBJ unless already known)."
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

(defun knessy--ci-lookup (idx kind ns name)
  (or (ht-get (knessy--ci-by-key idx) (list kind ns name))
      (when-let* ((resolver (knessy--ci-resolver idx)))
        (funcall resolver kind ns name))))

;; ---------------------------------------------------------------------------
;; Edges
;; ---------------------------------------------------------------------------

(defun knessy--linked-selector (obj)
  "The label selector of OBJ if it selects pods, else nil.
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
  "T if the label selector SELECTOR (matchLabels/matchExpressions) matches LABELS."
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
  "The (KIND NS NAME) triples that OBJ refers to.
References are found structurally, wherever they appear in the object, so that
pod templates of any workload kind (including CRDs) are covered."
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
                   ;; endpoints -> pod, hpa -> workload, pv -> pvc
                   ((or "targetRef" "scaleTargetRef" "claimRef")
                    (add (knessy--linked-get v "kind")
                         (or (knessy--linked-get v "namespace") ns)
                         (knessy--linked-get v "name")))))
                ((and (vectorp v) (equal k "imagePullSecrets"))
                 (seq-doseq (e v)
                   (add "Secret" ns (knessy--linked-get e "name"))))))))
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
  "Fill the reverse references table of IDX from the objects registered so far."
  (let ((referrers (knessy--ci-referrers idx)))
    (dolist (obj (ht-values (knessy--ci-by-uid idx)))
      (dolist (target (knessy--linked-forward-targets idx obj))
        (ht-set referrers target (cons obj (ht-get referrers target)))))))

(defun knessy--linked-owners (idx obj)
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
  (ht-get (knessy--ci-owned-by idx) (knessy--linked-uid obj)))

(defun knessy--linked-forward (idx obj)
  (delq nil
        (mapcar (lambda (target) (apply #'knessy--ci-lookup idx target))
                (knessy--linked-forward-targets idx obj))))

(defun knessy--linked-incoming (idx obj)
  (ht-get (knessy--ci-referrers idx) (knessy--linked-key obj)))

;; ---------------------------------------------------------------------------
;; The walk
;; ---------------------------------------------------------------------------

(defun knessy--linked-closure (starts next &optional exclude)
  "Objects reachable from STARTS by repeatedly applying NEXT, nearest first.
NEXT takes an object and returns a list of objects. Neither STARTS nor
EXCLUDE (both lists of objects) are part of the result."
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
  "All objects linked to ROOT (see the commentary on top of the file).
The result is ordered: ancestors (top-most first), ROOT, descendants,
forward references, incoming references."
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
                    forward)))
    (append (reverse ancestors) (list root) descendants forward incoming)))

;; ---------------------------------------------------------------------------
;; kubectl glue
;; ---------------------------------------------------------------------------

(defun knessy--linked-kubectl-json (args)
  "Run `kubectl ARGS -o json' in the current context and return the parsed output.
Signals an error if there's no output at all. Partial output is accepted,
because kubectl exits non-zero if it can't list just one of many types."
  (let ((buf (generate-new-buffer " *knessy-linked*" t))
        (errfile (make-temp-file "knessy-linked-stderr")))
    (unwind-protect
        (let ((cmd (s-concat knessy-kubectl " --context " knessy--context " " args " -o json")))
          (knessy--log 3 cmd)
          (with-environment-variables (("KUBECONFIG" knessy-kubeconfig))
            (call-process-shell-command cmd nil (list buf errfile) nil))
          (with-current-buffer buf
            (if (zerop (buffer-size))
                (error "kubectl %s failed: %s" args
                       (s-trim (with-temp-buffer
                                 (insert-file-contents errfile)
                                 (buffer-string))))
              (goto-char (point-min))
              (json-parse-buffer :null-object nil :false-object nil))))
      (kill-buffer buf)
      (delete-file errfile))))

(defun knessy--linked-api-resources ()
  "API resources of the current context, guaranteed to have :kind and :group.
Caches populated by older versions of Knessy don't have them."
  (let ((resources (knessy--api-resources)))
    (if (and resources (not (asoc-get (car resources) :kind)))
        (knessy--cache-set knessy--cache
                           (list :api-resources knessy--context)
                           (knessy--kubectl-api-resources-list))
      resources)))

(defun knessy--linked-find-resource (resources type)
  "The API resource whose name or kind is TYPE (a resource type or a kind)."
  (let ((candidates (-remove (lambda (r) (member (asoc-get r :name) knessy-linked-ignored-resource-types))
                             resources)))
    (or (-find (lambda (r) (equal (asoc-get r :name) type)) candidates)
        (-find (lambda (r) (equal (asoc-get r :kind) type)) candidates)
        (-find (lambda (r) (s-equals? (downcase (asoc-get r :kind)) (downcase type))) candidates))))

(defun knessy--linked-resource-type-of (resources obj)
  "Fully-qualified resource type name (as in `kubectl api-resources') of OBJ."
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

(defun knessy--linked-fetch-namespace (ns resources)
  "All objects of all the listable types in namespace NS."
  (let* ((types (->> resources
                     (-filter (lambda (r) (and (asoc-get r :namespaced)
                                               (member "list" (asoc-get r :verbs))
                                               (not (member (asoc-get r :name)
                                                            knessy-linked-ignored-resource-types)))))
                     (mapcar (lambda (r) (asoc-get r :name)))))
         (list (knessy--linked-kubectl-json
                (format "-n %s get %s" (shell-quote-argument ns) (s-join "," types)))))
    (append (knessy--linked-get list "items") nil)))

(defun knessy--linked-make-resolver (idx resources)
  "A resolver for IDX that fetches cluster-scoped objects (and only those) on demand."
  (let ((tried (ht)))
    (lambda (kind ns name)
      (when (and (null ns) kind name (not (ht-contains? tried (cons kind name))))
        (ht-set tried (cons kind name) t)
        (when-let* ((resource (knessy--linked-find-resource resources kind))
                    (obj (ignore-errors
                           (knessy--linked-kubectl-json
                            (format "get %s/%s" (asoc-get resource :name) (shell-quote-argument name))))))
          (knessy--ci-add idx obj))))))

(defun knessy--linked-ids (ns rt name)
  "IDs of all the resources linked to resource NAME of type RT in namespace NS.
NS is nil for cluster-scoped resources. The IDs are as in the other Knessy views,
i.e. (NS . (RT . NAME))."
  (let* ((resources (knessy--linked-api-resources))
         (root-resource (or (knessy--linked-find-resource resources rt)
                            (user-error "Unknown resource type %s" rt)))
         (idx (knessy--ci-make)))
    (setf (knessy--ci-resolver idx) (knessy--linked-make-resolver idx resources))
    (when ns
      (dolist (obj (knessy--linked-fetch-namespace ns resources))
        (knessy--ci-add idx obj))
      (knessy--linked-index-referrers idx))
    (let ((root (or (ht-get (knessy--ci-by-key idx)
                            (list (asoc-get root-resource :kind) ns name))
                    (knessy--ci-add
                     idx
                     (knessy--linked-kubectl-json
                      (format "%s get %s/%s"
                              (if ns (format "-n %s" (shell-quote-argument ns)) "")
                              (asoc-get root-resource :name)
                              (shell-quote-argument name)))))))
      (mapcar (lambda (obj)
                (cons (knessy--linked-ns obj)
                      (cons (knessy--linked-resource-type-of resources obj)
                            (knessy--linked-name obj))))
              (knessy--linked-graph idx root)))))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defun knessy--display-linked ()
  "Display the resources linked to `knessy--linked-root' in the current buffer.
This is synchronous: the graph is built from a handful of kubectl calls."
  (unless knessy--linked-root
    (user-error "No resource to show the linked resources of; use the jump command on a resource"))
  (let* ((ids (apply #'knessy--linked-ids knessy--linked-root))
         (items (ht))
         (view (ht-get knessy-views (cons knessy--resource-type knessy--view))))
    ;; `ht-items' hands the items back in reverse order of insertion
    (dolist (id (reverse ids))
      (ht-set items id (ht ("RESOURCE-TYPE" (knessy--id-rt id))
                           ("NAMESPACE" (or (knessy--id-ns id) ""))
                           ("NAME" (knessy--id-name id)))))
    (knessy--display2-common-part
     (current-buffer)
     view
     `(((:items . ,items)
        (:headers . ((:static . ("RESOURCE-TYPE" "NAMESPACE" "NAME"))))))
     nil)))

(provide 'knessy-linked)
