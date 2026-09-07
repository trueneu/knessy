;; -*- lexical-binding: t; -*-

(require 'knessy-utils)

;; TODO: include labels as filters here?
;; TODO: rename to kubectl get command and fix the verb?..
(defun knessy--kubectl-cmd (verb &optional fmt omit-namespace? no-headers? omit-context? omit-labels? omit-fields?)
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
  (let ((cmd (s-concat
              knessy-kubectl
              (s-concat " --context " knessy--context)
              (s-concat " -n " ns)
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
  (let ((knessy--resource-type "get-contexts"))
    (knessy--kubectl-cmd "config" "name" t nil t t t)))

(defun knessy--kubectl-get-namespaces-cmd ()
  (let ((knessy--resource-type "namespaces"))
    (knessy--kubectl-cmd "get" "custom-columns='NAME:.metadata.name'" t t nil t t)))

(defun knessy--kubectl-get-labels-cmd ()
  (knessy--log 4 "called knessy--kubectl-get-labels-cmd")
  (knessy--kubectl-cmd "get" "jsonpath='{range .items[*]}{.metadata.labels}{\"\\n\"}{end}'" nil t nil t t))

;; TODO (pgu, 17.05.2026): include resource-type as explicit argument here, to support multiple resource-type views
(defun knessy--kubectl-get-obj-cmd (ns rt name &optional fmt)
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
  (knessy--log 4 "called knessy--kubectl-describe-obj-cmd")
  (let ((knessy--resource-type (s-concat rt "/" name))
        (knessy--namespace ns))
    (knessy--kubectl-cmd "describe" nil nil nil nil t t)))

(defun knessy--kubectl-apply-file-cmd (filename)
  (knessy--log 4 "called knessy--kubectl-apply-file-cmd")
  (knessy--kubectl-file-cmd "apply" filename))

(defun knessy--kubectl-validate-file-cmd (filename)
  (knessy--log 4 "called knessy--kubectl-validate-file-cmd")
  (knessy--kubectl-file-cmd "apply" filename t))

(defun knessy--kubectl-delete-file-cmd (filename)
  (knessy--log 4 "called knessy--kubectl-delete-file-cmd")
  (knessy--kubectl-delete-cmd filename nil))

(comment
 (let ((knessy--context "k8s-local")
       (knessy--namespace "kube-system")
       (knessy--resource-type "pods")
       (name "traefik-c5c8bf4ff-crlxs"))
   (knessy--kubectl-get-obj-cmd name :yaml)))

(defun knessy--kubectl-call->cmd (call)
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
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "contexts" t t t)))))
    (knessy--shell-exec
     (knessy--kubectl-get-contexts-cmd)
     buf)
    (knessy--utils-read-buffer buf)))

(defun knessy--kubectl-namespaces ()
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "namespaces" nil t t)))))
    (knessy--shell-exec
     (knessy--kubectl-get-namespaces-cmd)
     buf)
    (knessy--utils-read-buffer buf)))

(defun knessy--kubectl-labels ()
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
  (s-concat knessy-kubectl
            " --context " knessy--context
            " api-resources -o json"))

(defun knessy--kubectl-api-resources-parse (buf)
  "Parse BUF (output of `kubectl api-resources -o json') into a list of
alists, one per resource type, each shaped like
`((:name . FULLY-QUALIFIED-NAME) (:namespaced . BOOL) (:verbs . (VERB ...)))'.
FULLY-QUALIFIED-NAME matches what `kubectl api-resources --output name'
prints (e.g. \"deployments.apps\", or just \"nodes\" for core resources)."
  (let* ((parsed (with-current-buffer buf
                   (goto-char (point-min))
                   (json-parse-buffer :object-type 'alist :array-type 'list)))
         (resources (alist-get 'resources parsed)))
    (mapcar
     (lambda (r)
       (let* ((name (alist-get 'name r))
              (group (alist-get 'group r)))
         `((:name . ,(if group (s-concat name "." group) name))
           (:namespaced . ,(eq t (alist-get 'namespaced r)))
           (:verbs . ,(alist-get 'verbs r)))))
     resources)))

(defun knessy--kubectl-api-resources-list ()
  "Run and parse `kubectl api-resources' once; see `knessy--kubectl-api-resources-parse'."
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "api-resources" nil t t)))))
    (knessy--shell-exec (knessy--kubectl-api-resources-cmd) buf)
    (prog1 (knessy--kubectl-api-resources-parse buf)
      (kill-buffer buf))))

(defun knessy--api-resources ()
  (knessy--cache-get
   knessy--cache
   (list :api-resources knessy--context)
   (lambda ()
     (knessy--kubectl-api-resources-list))))

(defun knessy--resource-types ()
  (cons knessy-all-namespaced-resource-types-string
        (cons knessy-all-cluster-resource-types-string
              (mapcar (lambda (r) (asoc-get r :name)) (knessy--api-resources)))))

(defun knessy--resource-types-set ()
  (knessy--utils-set
   (mapcar (lambda (r) (asoc-get r :name)) (knessy--api-resources))))

(defun knessy--resource-types-namespaced ()
  (knessy--utils-set
   (->> (knessy--api-resources)
        (-filter (lambda (r) (asoc-get r :namespaced)))
        (mapcar (lambda (r) (asoc-get r :name))))))

;; only resource types supporting the "list" verb can be safely combined into
;; a single "kubectl get type1,type2,..." call without the whole call failing
(defun knessy--resource-types-listable-namespaced ()
  (->> (knessy--api-resources)
       (-filter (lambda (r) (and (asoc-get r :namespaced)
                                 (member "list" (asoc-get r :verbs)))))
       (mapcar (lambda (r) (asoc-get r :name)))))

(defun knessy--resource-types-listable-cluster ()
  (->> (knessy--api-resources)
       (-filter (lambda (r) (and (not (asoc-get r :namespaced))
                                 (member "list" (asoc-get r :verbs)))))
       (mapcar (lambda (r) (asoc-get r :name)))))

(defun knessy--kubectl-all-namespaced-resource-types-joined ()
  "Comma-joined list of every listable namespaced resource type, for the *ALL-NS* view."
  (s-join "," (knessy--resource-types-listable-namespaced)))

(defun knessy--kubectl-all-cluster-resource-types-joined ()
  "Comma-joined list of every listable cluster-scoped resource type, for the *ALL-CLUSTER* view."
  (s-join "," (knessy--resource-types-listable-cluster)))

(defun knessy-cache-clear ()
  "Resets all the Knessy caches."
  (interactive)
  (ht-clear knessy--cache))

;; TODO: things different between the two functions below are
;; buffer names
;; kubectl commands
;; destinations
(defun knessy--cache-namespaces-populate-async ()
  (dolist (ctx (knessy--contexts))
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
  (dolist (ctx (knessy--contexts))
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
  (with-current-buffer buf
    (goto-char (point-min))
    (json-parse-buffer)))

(defun knessy--kubectl-get-object-parsed-sync (ns rt name)
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
  (knessy--shell-exec-async3
    (knessy--kubectl-log-cmd ns resource-type name follow? tail nil nil all-containers? t container t)
    buf
    buferr
    (knessy--make-process-filter-logs2 buf prefix? knessy-prefix-width timestamps?)
    ;; (knessy--make-process-filter-logs buf name knessy-prefix? knessy-prefix-width)
    (lambda ())))

(defun knessy--kubectl-log-labels-async (labels parallel buf buferr follow? tail all-containers? prefix? container timestamps?)
  (knessy--shell-exec-async3
    (knessy--kubectl-log-cmd nil nil follow? tail parallel labels all-containers? prefix? container timestamps?)
    buf
    buferr
    (knessy--make-process-filter-logs buf nil nil nil)
    (lambda ())))

(defun knessy--kubectl-delete-object-async (ns resource-type name buf buferr force? grace-period now?)
  (knessy--shell-exec-async2
    (knessy--kubectl-delete-object-cmd ns resource-type name force? grace-period now?)
    buf
    buferr
    (lambda () (message "Object(s) deleted!"))))

(defun knessy--kubectl-get-object-sync (ns rt name buf &optional fmt)
  (knessy--shell-exec
   (knessy--kubectl-get-obj-cmd ns rt name fmt)
   buf))

;; TODO (pgu, 20.05.2026): lord why this creates its own buffer but the others get it as a param?
(defun knessy--kubectl-describe-object-sync (ns rt name)
  (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name name)))))
    (knessy--shell-exec
     (knessy--kubectl-describe-obj-cmd ns rt name)
     buf)
    buf))

(defun knessy--kubectl-apply-file-sync (buf filename)
  (knessy--shell-exec
   (knessy--kubectl-apply-file-cmd filename)
   buf))

(defun knessy--kubectl-delete-file-sync (buf filename)
  (knessy--shell-exec
   (knessy--kubectl-delete-file-cmd filename)
   buf))

(defun knessy--kubectl-validate-file-sync (buf filename)
  (knessy--shell-exec
   (knessy--kubectl-validate-file-cmd filename)
   buf
   t))

(defun knessy--cache-labels-populate-async (ctx ns resource-type)
  (knessy--log 3 "In knessy--cache-labels-populate-async")
  ;; labels are per-resource-type; there's no single "get labels" call that
  ;; makes sense across every kind at once, and label filtering is refused
  ;; for the *ALL-...* pseudo-types anyway, so skip warming the cache
  (unless (knessy--resource-type-all-synthetic? resource-type)
    (let ((buf (knessy--utils-make-buffer
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
          ;; TODO: maybe (knessy--namespace-all?) should take no arguments
          (if (or (knessy--namespace-all? ns)  ;; FIXME: knessy--namespace is "default" here? because buffer-local?
                  (knessy--resource-type-global? resource-type))
              (list :labels knessy--context knessy--resource-type)
            (list :labels knessy--context knessy--namespace knessy--resource-type))
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
  "Rebuild all the main caches."
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
