;; -*- lexical-binding: t; -*-

;; TODO: test on a cluster with metrics plugin enabled (minikube?)
(defun knessy--make-convert-and-add-reduce-fn (conversion-fn)
  (lambda (acc val)
    (let ((converted (funcall conversion-fn val)))
      (+ acc converted))))

(defcustom knessy-default-column-width
  12
  "Default column width."
  :type 'integer
  :group 'knessy)

;; TODO: make this an alist?
(defcustom knessy-column-widths
  (ht ("NAME" 32)
      ("NAMESPACE" 16)
      ("READY" 5)
      ("RDY" 5)
      ("IP" 16)
      ("STATUS" 12)
      ("RESTARTS" 20)
      ("NODE" 25)
      ("CPU(r)" 30)
      ("MEM(r)" 30)
      ("CPU(l)" 30)
      ("MEM(l)" 30))
  "Column widths by column name."
  :type 'sexp
  :group 'knessy)

;; TODO: make all structures alists again?.. since they're faster than ht, except items collection maybe

;; FIXME: this won't get updated when knessy-default-view-string is updated...
(defcustom knessy-default-view-alist
  `(("pods" . "concise"))
  "ALIST of resource resource-type to the view name that's applied by default")

;; (defcustom knessy-default-view
;;   '(:calls . (((:type . :get))))
;;   "An alist depicting the default view"
;;   :type 'sexp
;;   :group 'knessy)

;; TODO: verify that if at least one container doesn't have requests/limits set, pod shows "n/a"
;; TODO: if a container is dead with Init:Error (or most likely isn't running for any other reason) it won't even appear in the top. Hence it will be missing from the output
;; FIXME: add views for deployments, rs, etc -- to propertize DESIRED/CURRENT number of replicas -- this could require tablist entry generation changes
(defcustom knessy-views
  (ht ('("pods" . "top")
       `((:columns . ("NAME" "RDY" "STATUS" "RESTARTS" "CPU(r)" "CPU(l)" "MEM(r)" "MEM(l)" "NODE" "AGE"))
         (:column-rename . ,(ht ("RDY" "READY")))
         (:widths . ,(ht ("NAME" 32)))
         (:calls . (((:type . :get-wide))
                    ((:type . :jsonpath)
                     (:spec . "'{range .items[*]}{.metadata.namespace}|{.metadata.name}{range .spec.containers[*]}|{.resources.requests.cpu}|{.resources.requests.memory}|{.resources.limits.cpu}|{.resources.limits.memory}{end}{\"\\n\"}{end}'")
                     (:headers . ((:static . ("NAMESPACE" "NAME"))
                                  (:repeated . ("CPUREQ" "MEMREQ" "CPULIM" "MEMLIM"))))
                     (:pre-process . ,(ht ("CPUREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))
                                                      (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "CPUREQNOTSET" t))))))
                                          ("CPULIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))
                                                      (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "CPULIMNOTSET" t))))))
                                          ("MEMREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))
                                                      (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "MEMREQNOTSET" t))))))
                                          ("MEMLIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))
                                                      (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "MEMLIMNOTSET" t))))))))
                     (:post-process . ,(ht ("CPUREQ" #'knessy--convert-cpu-units-str)
                                           ("CPULIM" #'knessy--convert-cpu-units-str)
                                           ("MEMREQ" #'knessy--convert-size-units-str)
                                           ("MEMLIM" #'knessy--convert-size-units-str)))
                     (:post-process-item . ,(lambda (item) nil)))
                    ((:type . :top)
                     (:pre-process . ,(ht ("CPU(cores)" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))))
                                          ("MEMORY(bytes)" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))))))
                     (:post-process . ,(ht ("CPU(cores)" #'knessy--convert-cpu-units-str)
                                           ("MEMORY(bytes)" #'knessy--convert-size-units-str))))))

         (:post-process-item . ,(lambda (item)
                                        (let ((cpu-usage-str (ht-get item "CPU(cores)" "-"))
                                              (cpu-req-str (ht-get item "CPUREQ" "-")))
                                          (ht-set item "CPU(r)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-cpu-units-millis cpu-usage-str)
                                                                  (knessy--convert-cpu-units-millis cpu-req-str))
                                                                 " ("
                                                                 cpu-usage-str
                                                                 "/"
                                                                 cpu-req-str
                                                                 ")")))
                                        (let ((mem-usage-str (ht-get item "MEMORY(bytes)" "-"))
                                              (mem-req-str (ht-get item "MEMREQ" "-")))
                                          (ht-set item "MEM(r)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-size-units-bytes mem-usage-str)
                                                                  (knessy--convert-size-units-bytes mem-req-str))
                                                                 " ("
                                                                 mem-usage-str
                                                                 "/"
                                                                 mem-req-str
                                                                 ")")))
                                        (let ((cpu-usage-str (ht-get item "CPU(cores)" "-"))
                                              (cpu-lim-str (ht-get item "CPULIM" "-")))
                                          (ht-set item "CPU(l)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-cpu-units-millis cpu-usage-str)
                                                                  (knessy--convert-cpu-units-millis cpu-lim-str))
                                                                 " ("
                                                                 cpu-usage-str
                                                                 "/"
                                                                 cpu-lim-str
                                                                 ")")))
                                        (let ((mem-usage-str (ht-get item "MEMORY(bytes)" "-"))
                                              (mem-lim-str (ht-get item "MEMLIM" "-")))
                                          (ht-set item "MEM(l)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-size-units-bytes mem-usage-str)
                                                                  (knessy--convert-size-units-bytes mem-lim-str))
                                                                 " ("
                                                                 mem-usage-str
                                                                 "/"
                                                                 mem-lim-str
                                                                 ")")))))))
      ('("pods" . "concise")
       `((:columns . ("NAME" "RDY" "STATUS" "RESTARTS" "IP" "NODE" "AGE"))
         (:column-rename . ,(ht ("RDY" "READY")))
         (:calls . (((:type . :get-wide)))))))


  "The variable defines different queries by resource type."
  :type 'sexp
  :group 'knessy)

(comment
 (knessy--convert-size-units-bytes "400Mi")
 (funcall
   (->
    knessy-views
    (ht-get "pods")
    (asoc-get :calls)
    (first)
    (asoc-get :pre-process)
    (ht-get "CPUREQ"))
   0
   1))

;; TODO (pgu, 17.05.2026): this should be recomputed every time views change
(defvar knessy--views-by-resource-type
  (let ((res (ht)))
   (dolist (resource-type-view (ht-keys knessy-views))
     (let ((resource-type (car resource-type-view))
           (view (cdr resource-type-view)))
       (unless (ht-contains? res resource-type)
         (ht-set res resource-type '()))
       (ht-update-with! res resource-type (lambda (l) (cons view l)))))
   (dolist (resource-type (ht-keys res) res)
     (ht-update-with! res resource-type (lambda (l) (cons knessy-default-view-string l)))))

  "resource-type->views-list hashtable")

(comment)



;; (defcustom knessy-views
;;   (ht ("pods" `((:columns . ("NAME" "NODE"))
;;                 (:calls . (((:type . :custom-columns)
;;                             (:spec . "NAMESPACE:.metadata.namespace,NAME:.metadata.name"))
;;                            ((:type . :get-wide)))))))

;;   "The variable defines different queries by resource type."
;;   :type 'sexp
;;   :group 'knessy)

(defcustom knessy-call-default-type :get
  "Default Knessy call type."
  :type '(choice (const :tag "get" :get)
                 (const :tag "get wide" :get-wide))
  :group 'knessy)

(provide 'knessy-views)
