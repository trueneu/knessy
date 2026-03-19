;; -*- lexical-binding: t; -*-

(defcustom knessy-queries
  (ht ("pods" `((:columns . ("NAMESPACE" "NAME" "STATUS" "READY"))
                (:calls . (((:type . :custom-columns)
                            (:spec . "NAMESPACE:.metadata.namespace,NAME:.metadata.name"))
                           ((:type . :get-wide)))))))

  "The variable defines different queries by resource type."
  :type 'sexp
  :group 'knessy)

(defcustom knessy-query-default-type 'get
  "Default Knessy query type."
  :type '(choice (const :tag "get" get)
                 (const :tag "get wide" get-wide))
  :group 'knessy)

(provide 'knessy-queries)
