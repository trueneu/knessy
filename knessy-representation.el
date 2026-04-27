;; -*- lexical-binding: t; -*-

;; dependencies: dash, asoc, ht, s

(defun knessy--debug-print-ht (table)
  (dolist (item (ht-items table))
    (princ item)))

(defun knessy--update-columns-max-width (table k value)
  (ht-update-with!
   table
   k
   (lambda (current)
     (max current (length value)))
   ;; the default is just the length of the column name itself
   (length k)))

(comment
 (knessy--parse-table-kubectl-output (get-buffer "*knessy-aio-display*<35>"))
 (knessy--parse-table-kubectl-output
  (get-buffer "*knessy-aio-display*<41>")
  '((:static . ("NAMESPACE" "NAME"))
    (:repeated . ("CPUREQ" "MEMREQ" "CPULIM" "MEMLIM")))
  (ht ("CPUREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))
                  (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "CPUREQNOTSET" t))))))
      ("CPULIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))
                  (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "CPULIMNOTSET" t))))))
      ("MEMREQ" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))
                  (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "MEMREQNOTSET" t))))))
      ("MEMLIM" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))
                  (:mixin-fn . ,(lambda (v) (when (s-blank? v) (cons "MEMLIMNOTSET" t)))))))))

(cl-defun knessy--parse-table-kubectl-output
    (buf &optional headers (pre-process-ht (ht)) (post-process-ht (ht)) post-process-fn)
  "Parses buffer BUF of information that was output by a kubectl command,
into a datastructure to be manipulated further down the road. The result looks
like this:
((:items . hash-table((ns . name) ((key1 . val1)
                                  (key2 . val2)
                                  ...)))
 (:headers . ((:static . (...)
              (:repeated . (...))
              (:widths (ht (key1 width1) (key2 width2) ...))))))

BUF is the buffer with info present. It should only have output from one kubectl call.
HEADERS is an alist of form ((:static . (COL1 COL2 ...))
                             (:repeated . (REPCOL1 REPCOL2 ...)))
Static columns appear once in the beginning of the values.
Repeated columns appear as many times as they should at the end, in looped fashion
(REPCOL1 REPCOL2 REPCOL1 REPCOL2 ...)
This is useful when dealing with complex queries including loops with unknown number of
elements (e.g. output memory limit for every container in a pod).
PRE-PROCESS-HT is a hashtable of form (COL1 ((:fn . reduce-func)
                                             (:acc . initial-acc)))
Every column specified in the table will be reduced with initial-acc as initial accumulator.
Useful for repeated columns output, where you need to sum up all the values, or similar.
Specify empty hashtable if no pre-processing is desired.
POST-PROCESS-HT is a hashtable of form (COL1 func)
Every column specified will be modified by applying func to its value.
Specify empty hashtable if no post-processing is desired.
"
  (knessy--log 4 (format "Started parsing buffer %s" (buffer-name buf)))
  (if (zerop (buffer-size buf))
      nil
    (let ((header-static)
          (header-repeated)
          (items-ht (ht))
          (widths-ht (ht))
          ;; TODO: this looks kinda ugly, maybe just pass it in a fat "context" variable?
          (knessy--context-orig knessy--context)
          (knessy--namespace-orig knessy--namespace)
          (knessy--resource-type-orig knessy--resource-type)
          (knessy--namespace-current-all?-orig knessy--namespace-current-all?))
      (with-current-buffer buf
        (setq knessy--context knessy--context-orig)
        (setq knessy--namespace knessy--namespace-orig)
        (setq knessy--resource-type knessy--resource-type-orig)
        ;; TODO: maybe just call a function there
        (setq knessy--namespace-current-all? knessy--namespace-current-all?-orig)
        (goto-char (point-min))

        (if (not headers)
            ;; if we have to parse headers ourselves
            (progn
              ;; TODO: this is super-brittle
              (setq header-static (s-split (rx (>= 2 whitespace))
                                           (s-trim (thing-at-point 'line t))))
              (forward-line 1))
          (setq header-static (asoc-get headers :static))
          (setq header-repeated (asoc-get headers :repeated)))

        (while (not (eobp))
          (knessy--log 5 (format "Working on line: %s" (thing-at-point 'line t)))
          (let* ((values (s-split (rx (or (>= 2 whitespace) "|"))
                                  (s-trim (thing-at-point 'line t))))
                 (item (ht))
                 (name)
                 ;; TODO: here should be the check if resource-type is namespaced or not
                 (resource-type knessy--resource-type)
                 (namespace (if (ht-get (knessy--cache-get knessy--cache (list :resource-types-namespaced knessy--context)
                                                           (lambda ()
                                                             (knessy--utils-set
                                                              (knessy--utils-read-buffer buf-global))))
                                        resource-type
                                        nil)
                                knessy--namespace
                              nil))

                 (accumulators (ht))
                 (mixins (ht)))
            (dolist (pair (-zip-pair (-concat header-static
                                              (-cycle header-repeated))
                                     values))
              (let* ((key (car pair))
                     (value (cdr pair))
                     (pre-process-asoc (ht-get pre-process-ht key nil))
                     (pre-process-reduce-fn (asoc-get pre-process-asoc :reduce-fn nil))
                     (pre-process-reduce-acc (asoc-get pre-process-asoc :reduce-acc 0))
                     (pre-process-mixin-fn (asoc-get pre-process-asoc :mixin-fn nil)))
                ;; if mixin is enabled, call the fn and put the result into a separate hashtable
                (if pre-process-mixin-fn
                    (let* ((mixin (funcall pre-process-mixin-fn value)))
                      (if mixin
                          (let ((mixin-key (car mixin))
                                (mixin-value (cdr mixin)))
                            (ht-set mixins mixin-key mixin-value)))))

                ;; if pre-processing is enabled for the key, don't put it in the result yet
                (if pre-process-reduce-fn
                    (let* ((acc (ht-get accumulators key pre-process-reduce-acc))
                           (new-acc (funcall pre-process-reduce-fn acc value)))
                      (ht-set accumulators key new-acc))
                  ;; if key is "simple", put as is
                  ;; probably only add it if it's not name/namespace
                  (ht-set item key value)
                  ;; TODO: redefine resource-type here when we support the *ALL* resource-types queries
                  ;; TODO: this effectively means we can't pre-process NAME, NAMESPACE or KIND
                  (cond ((s-equals? key "NAME")
                         (setq name value))
                        ;; TODO: if namespace is missing from the output, must grab "current" one
                        ;; but only if it's a namespaced resource
                        ((s-equals? key "NAMESPACE")
                         (setq namespace value))))))

            ;; then add all the accumulated goodies
            (dolist (kv (ht-items accumulators))
              (ht-set item (car kv) (cadr kv)))
            ;; and the mixins!
            (dolist (kv (ht-items mixins))
              (ht-set item (car kv) (cadr kv)))
            ;; post-process columns
            (dolist (kv (ht-items post-process-ht))
              (let ((k (car kv))
                    (fn (cadr kv)))
                (ht-update-with! item k fn)))
            ;; post-process item
            (when post-process-fn
              (funcall post-process-fn item))
            ;; update widths for strings
            ;; FIXME: if it's needed, it can be returned; + result :headers :widths HASHTABLE
            ;; (dolist (kv (ht-items item))
            ;;   (let ((k (car kv))
            ;;         (v (cadr kv)))
            ;;     (when (stringp v)
            ;;       (knessy--update-columns-max-width widths-ht k v))))
            ;; finally add it to the resulting hashtable
            (ht-set items-ht (cons namespace (cons resource-type name)) item))
          (forward-line 1)))
      (let ((result `((:items . ,items-ht)
                      (:headers . ((:static . ,header-static)
                                   (:repeated . ,header-repeated))))))
        ;; TODO: come up with debug toggles to turn this off and on again
        ;; (message "Parsed results:")
        ;; (princ result)
        (knessy--log 4 (format "Finished parsing %s" (buffer-name buf)))
        (knessy--log 5 "Returning parsed results:")
        (knessy--log 5 result)
        result))))


(comment
 (aio-wait-for (knessy--aio-display))
 (knessy--shell-exec-parse "kubectl get pods -n kube-system" (get-buffer-create "*test-call-and-parse*"))
 (knessy--parse-table-kubectl-output
  (get-buffer "test-parse-simple"))
 (knessy--parse-table-kubectl-output
  (get-buffer "test-parse-post-process")
  nil
  (ht)
  (ht ("VALUE" (lambda (s) (string-to-number s)))))

 (knessy--parse-table-kubectl-output
  (get-buffer "test-parse-pre-process")
  '((:static . ("NAME" "NAMESPACE"))
    (:repeated . ("VALUE1" "VALUE2")))
  (ht ("VALUE1" `((:fn . ,(lambda (v s) (+ v (string-to-number s))))
                  (:acc . 0)))
      ("VALUE2" `((:fn . ,(lambda (v s) (* v (string-to-number s))))
                  (:acc . 1))))
  (ht ("VALUE1" (lambda (i) (number-to-string i)))
      ("VALUE2" (lambda (i) (number-to-string i))))))

;; TODO: not the most efficient way it seems...
(defun knessy--merge-items (items-ht &rest more)
  (dolist (table more items-ht)
    (dolist (k (ht-keys items-ht))
      (ht-update-with! items-ht k
                       (lambda (item-entry)
                         (ht-merge item-entry (ht-get table k (ht))))))))

(comment
 (let* ((item1 (ht (:key1 :value1)))
        (item2 (ht (:key2 :value2)))
        (ht1 (ht (:item item1)))
        (ht2 (ht (:item item2))))
   (knessy--merge-items ht1 ht2)))

;; TODO: we're going to need to pre-compute the column-numbers beforehand
;; to implement multi-column sorting, instead of calculating them as we go
;; but maybe we don't have to do that as tabulated-list-mode sorting is stable
;; although the precomputed variant is more flexible
;; in any case this has to happen in the "collate multiple queries results" phase
(defun knessy--make-tablist-format (columns widths)
  (knessy--log 3 "In knessy--make-tablist-format")
  (let ((column-counter 0))
    (apply
     #'vector
     (mapcar
      (lambda (column)
        (prog1
          (list column
                (ht-get widths column knessy-default-column-width)
                (cond ((s-equals? "RESTARTS" column)
                       (knessy--comparator-make-time column-counter))
                      ((s-equals? "AGE" column)
                       (knessy--comparator-make-time column-counter))
                      ((s-equals? "RDY" column)
                       (knessy--comparator-make-ready column-counter))
                      ((s-equals? "CPU(r)" column)
                       (knessy--comparator-make-percentage column-counter))
                      ((s-equals? "CPU(l)" column)
                       (knessy--comparator-make-percentage column-counter))
                      ((s-equals? "MEM(r)" column)
                       (knessy--comparator-make-percentage column-counter))
                      ((s-equals? "MEM(l)" column)
                       (knessy--comparator-make-percentage column-counter))
                      (t
                       t)))
          (setq column-counter (1+ column-counter))))
      columns))))

(defun knessy--propertize-name (id name)
  (if (ht-get knessy--marked id nil)
      ;; TODO: face should be customizable
      (progn
        ;; (princ "Marking: ")
        ;; (princ id)
        ;; (princ "\n")
        (propertize name 'face 'dired-marked))
    name))

(defun knessy--make-tablist-entries (columns rename items)
  (knessy--log 3 "In knessy--make-tablist-entries")
  (mapcar
   (lambda (item)
     (let ((id (car item))
           (table (cadr item)))
       (list
        id
        (apply
         #'vector
         (mapcar
          (lambda (column)
            ;; TODO: maybe extract this to a separate dispatch table of some sorts
            (let ((value (ht-get table (ht-get rename column column) "??")))
              (cond
               ((s-equals? "NAME" column)
                (knessy--propertize-name id value))
               (t value))))
          columns)))))
   (ht-items items)))

;; TODO: most likely this will be called in the target buffer already
(defun knessy--make-tablist (columns rename items widths)
  (setq tabulated-list-format (knessy--make-tablist-format columns widths))
  (setq tabulated-list-entries (knessy--make-tablist-entries columns rename items))
  (tabulated-list-init-header)
  (unless knessy--table-remember-pos
    ;; FIXME: if already sorted by NAME, this changes sorting direction
    (tabulated-list-sort (-elem-index "NAME" columns)))
  (tabulated-list-print knessy--table-remember-pos)
  (setq knessy--table-remember-pos t))

(comment
 (knessy--make-tablist '("NAME" "NAMESPACE") nil)
 (setq knessy--columns-max-width
       (ht ("NAME" 6)
           ("NAMESPACE" 3)
           ("VALUE" 10)))
 (knessy--make-tablist
  '("NAMESPACE" "NAME" "VALUE")
  (ht ((cons "ns1" "obj1")
       '(("NAME" . "obj1")
         ("NAMESPACE" . "ns1")
         ("VALUE" . "123")))
      ((cons "ns1" "obj2")
       '(("NAME" . "obj2")
         ("NAMESPACE" . "ns1")
         ("VALUE" . "4567")))))

 (knessy--make-tablist-entries
  '("VALUE" "NAME" "NAMESPACE")
  (ht ((cons "ns1" "obj1")
       '(("NAME" . "obj1")
         ("NAMESPACE" . "ns1")
         ("VALUE" . "123")))
      ((cons "ns1" "obj2")
       '(("NAME" . "obj2")
         ("NAMESPACE" . "ns1")
         ("VALUE" . "4567"))))))

(provide 'knessy-representation)
