;; -*- lexical-binding: t; -*-

;; TODO: add pre-post-processing
;; TODO: test on a cluster with metrics plugin enabled (minikube?)
(defun knessy--make-convert-and-add-reduce-fn (conversion-fn)
  (lambda (acc val)
    (let ((converted (funcall conversion-fn val)))
      (message (format "converted %s to %d!" val converted))
      (+ acc converted))))

(defcustom knessy-default-column-width
  5
  "Default column width."
  :type 'integer
  :group 'knessy)

(defcustom knessy-column-widths
  (ht ("NAME" 32))
  "Column widths by column name."
  :type 'sexp
  :group 'knessy)

;; TODO: idea: introduce column-rename kv, just to rename stuff from default :get call
(defcustom knessy-views
  (ht ("pods" `((:columns . ("NAME" "CPU(r)" "CPU(l)" "MEM(r)" "MEM(l)"))
                (:widths . ,(ht ("NAME" 10)))
                (:calls . (((:type . :jsonpath)
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
                            (:post-process-item . ,(lambda (item) (message "Called post-process for an item!"))))))
                (:post-process-item . ,(lambda (item)
                                         (ht-set item "CPU(r)"
                                                 (if (ht-get item "CPUREQNOTSET" nil)
                                                     "?"
                                                   (ht-get item "CPUREQ")))
                                         (ht-set item "MEM(r)"
                                                 (if (ht-get item "MEMREQNOTSET" nil)
                                                     "?"
                                                   (ht-get item "MEMREQ")))
                                         (ht-set item "CPU(l)"
                                                 (if (ht-get item "CPULIMNOTSET" nil)
                                                     "?"
                                                   (ht-get item "CPULIM")))
                                         (ht-set item "MEM(l)"
                                                 (if (ht-get item "MEMLIMNOTSET" nil)
                                                     "?"
                                                   (ht-get item "MEMLIM"))))))))


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
