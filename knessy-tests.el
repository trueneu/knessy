;; this should be rewritten properly  -*- lexical-binding: t; -*-

(knessy--expand-colons "~/first:~/second:~/third")

(comment
 (let ((buf (find-file-noselect "test-parse-repeated-columns")))
   (message (buffer-name buf))
   (knessy--parse-table-kubectl-output
    buf
    '((:static . ("NAMESPACE" "NAME"))
      (:repeated . ("CPUREQ" "MEMREQ" "CPULIM" "MEMLIM")))
    (ht ("CPUREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))
                    (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "CPUREQNOTSET" t))))))
        ("CPULIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))
                    (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "CPULIMNOTSET" t))))))
        ("MEMREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))
                    (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "MEMREQNOTSET" t))))))
        ("MEMLIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))
                    (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "MEMLIMNOTSET" t)))))))
    (ht ("CPUREQ" #'knessy--convert-cpu-units-str)
        ("CPULIM" #'knessy--convert-cpu-units-str)
        ("MEMREQ" #'knessy--convert-size-units-str)
        ("MEMLIM" #'knessy--convert-size-units-str))
    (lambda (item)
      (when (ht-get item "CPULIMNOTSET" nil)
        (message "CPULIMIT NOT SET!"))))))

(comment
 (funcall
  (knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis)
  0
  "20m"))

(provide 'knessy-tests)
