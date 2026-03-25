;; -*- lexical-binding: t; -*-

;; TODO: add pre-post-processing
;; TODO: test on a cluster with metrics plugin enabled (minikube?)
(defcustom knessy-views
  (ht ("pods" `((:columns . ("NAME"))
                (:calls . (((:type . :jsonpath)
                            (:spec . "'{range .items[*]}{.metadata.namespace}  {.metadata.name}{range .spec.containers[*]}  {.resources.requests.cpu}  {.resources.requests.memory}  {.resources.limits.cpu}  {.resources.limits.memory}{end}{\"\\n\"}{end}'")
                            (:headers . ((:static . ("NAMESPACE" "NAME"))
                                         (:repeated . ("CPUREQ" "MEMREQ" "CPULIM" "MEMLIM"))))))))))


  "The variable defines different queries by resource type."
  :type 'sexp
  :group 'knessy)

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
