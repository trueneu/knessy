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
      ("STATUS" 12)
      ("RESTARTS" 15)
      ("NODE" 15)
      ("CPU(r)" 20)
      ("MEM(r)" 20)
      ("CPU(l)" 20)
      ("MEM(l)" 20))
  "Column widths by column name."
  :type 'sexp
  :group 'knessy)

;; TODO: make all structures alists again?.. since they're faster than ht, except items collection maybe

;; TODO: make a kind -> view table, allow multiple views per kind

;; FIXME: this won't get updated when knessy-view-default is updated...
(defcustom knessy-default-view-alist
  `(("pods" . ,knessy-view-default))
  "ALIST of resource kind to the view name that's applied by default")

(defvar knessy--views-last-selected (ht)
  "Contains the last selected view for a kind")

;; (defcustom knessy-default-view
;;   '(:calls . (((:type . :get))))
;;   "An alist depicting the default view"
;;   :type 'sexp
;;   :group 'knessy)

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
                                                                 ")"))))))))


  "The variable defines different queries by resource type."
  :type 'sexp
  :group 'knessy)

(comment
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

(defvar knessy--views-by-kind
  (let ((res (ht)))
   (dolist (kind-view (ht-keys knessy-views))
     (let ((kind (car kind-view))
           (view (cdr kind-view)))
       (unless (ht-contains? res kind)
         (ht-set res kind '()))
       (ht-update-with! res kind (lambda (l) (cons view l)))))
   (dolist (kind (ht-keys res) res)
     (ht-update-with! res kind (lambda (l) (cons knessy-view-default l)))))

  "kind->views-list hashtable")

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
