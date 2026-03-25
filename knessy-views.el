;; -*- lexical-binding: t; -*-

;; TODO: add pre-post-processing
;; TODO: test on a cluster with metrics plugin enabled (minikube?)
(defun knessy--make-convert-and-add-reduce-fn (conversion-fn)
  (lambda (acc val)
    (let ((converted (funcall conversion-fn val)))
      (message (format "converted %s to %d!" val converted))
      (+ acc converted))))

(defcustom knessy-views
  (ht ("pods" `((:columns . ("NAME"))
                (:calls . (((:type . :jsonpath)
                            (:spec . "'{range .items[*]}{.metadata.namespace}|{.metadata.name}{range .spec.containers[*]}|{.resources.requests.cpu}|{.resources.requests.memory}|{.resources.limits.cpu}|{.resources.limits.memory}{end}{\"\\n\"}{end}'")
                            (:headers . ((:static . ("NAMESPACE" "NAME"))
                                         (:repeated . ("CPUREQ" "MEMREQ" "CPULIM" "MEMLIM"))))
                            (:pre-process . ,(ht ("CPUREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))))
                                                 ("CPULIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))))
                                                 ("MEMREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))))
                                                 ("MEMLIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))))))
                            (:post-process . ,(ht ("CPUREQ" #'knessy--convert-cpu-units-str)
                                                  ("CPULIM" #'knessy--convert-cpu-units-str)
                                                  ("MEMREQ" #'knessy--convert-size-units-str)
                                                  ("MEMLIM" #'knessy--convert-size-units-str)))))))))


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
