;; -*- lexical-binding: t; -*-

(defcustom knessy-cache-ttl-default (* 3600 24)
  "Default cache time-to-live."
  :type 'integer
  :group 'knessy) ;; 1 day

;; TODO: don't know how to architect this yet...

(defun knessy--get-cached-data-ht (table key refresh-fn ttl))

(defun knessy--get-cached-data (var refresh-fn ttl)
  (let* ((now (current-time))
         (refresh-at (asoc-get var "refresh-at" '(0 0 0 0)))
         (data (asoc-get var "data" nil))
         (new-data (if (or (not data) (time-less-p refresh-at now))
                       (funcall refresh-fn)
                     data)))
    new-data))


(defun knessy--test (var)
  (asoc-put! var "key" "value"))

(comment
 (let ((l '((a . b))))
   (knessy--test l)
   (symbol-value 'l)))

(defun knessy--load-file-cached (path load-fn cache-for)
  (let* ((contents-str (if (f-file? path)
                           (f-read-text path)
                         "{\"refresh-at\": [0, 0, 0, 0], \"data\":{}}"))
         (contents (json-parse-string contents-str))
         (data (ht-get contents "data"))
         (refresh-at (append (ht-get contents "refresh-at") nil))
         (ctime (current-time)))
    (if (time-less-p refresh-at ctime)
        (let* ((new-data (funcall load-fn))
               (new-contents (ht-create)))
          (ht-set! new-contents "refresh-at" (time-add ctime cache-for))
          (ht-set! new-contents "data" new-data)
          (f-write-text (json-encode new-contents) 'utf-8 path)
          new-data)
      data)))

(provide 'knessy-cache)
