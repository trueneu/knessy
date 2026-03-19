;; -*- lexical-binding: t; -*-

;; TODO: make NAMESPACE hide-able (or force-appendable)
(defcustom knessy-views
  (ht ("pods" `((:columns . ("NAME" "STATUS" "READY"))
                (:calls . (((:type . :custom-columns)
                            (:spec . "NAMESPACE:.metadata.namespace,NAME:.metadata.name"))
                           ((:type . :get-wide)))))))

  "The variable defines different queries by resource type."
  :type 'sexp
  :group 'knessy)

(defcustom knessy-call-default-type 'get
  "Default Knessy call type."
  :type '(choice (const :tag "get" get)
                 (const :tag "get wide" get-wide))
  :group 'knessy)

(provide 'knessy-views)
