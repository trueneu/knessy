;; -*- lexical-binding: t; -*-

;; dependencies: dash, asoc, ht, s

;; TODO: not local?.. it probably could be: just not during the dev phase, it's inconvenient
;; TODO: could be needed to distinct between output buffers, but maybe not?
(defvar knessy--columns-max-width
  (ht)
  "Holds maximum width for any given column")

(defun knessy--reset-columns-max-width ()
  (ht-clear knessy--columns-max-width))

(defun knessy--debug-print-ht (table)
  (dolist (item (ht-items table))
    (princ item)))

(defun knessy--update-columns-max-width (k value)
  (ht-update-with!
   knessy--columns-max-width
   k
   (lambda (current)
     (max current (length value)))
   ;; the default is just the length of the column name itself
   (length k)))

(cl-defun knessy--parse-table-kubectl-output
    (buf &optional headers (pre-process-ht (ht)) (post-process-ht (ht)))
  "Parses buffer BUF of information that was output by a kubectl command,
into a datastructure to be manipulated further down the road. The result looks
like this:
((:items . hash-table((ns . name) ((key1 . val1)
                                  (key2 . val2)
                                  ...)))
 (:headers . ((:static . (...)
              (:repeated . (...))))))

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
  (knessy--reset-columns-max-width)
  (let ((header-static)
        (header-repeated)
        (items-ht (ht)))
    (with-current-buffer buf
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
        (let ((values (s-split (rx (>= 2 whitespace))
                               (s-trim (thing-at-point 'line t))))
              (item (asoc-make))
              (name)
              ;; TODO: here should be the check if kind is namespaced or not
              (namespace knessy--namespace)
              (kind knessy--kind)
              (accumulators (ht)))

          (dolist (pair (-zip-pair (-concat header-static
                                            (-cycle header-repeated))
                                   values))
            (let* ((key (car pair))
                   (value (cdr pair))
                   (pre-process-asoc (ht-get pre-process-ht key nil))
                   (pre-process-fn (asoc-get pre-process-asoc :fn))
                   (pre-process-acc (asoc-get pre-process-asoc :acc 0)))
              ;; if pre-processing is enabled for the key, don't put it in the result yet
              (if pre-process-fn
                  (let* ((acc (ht-get accumulators key pre-process-acc))
                         (new-acc (funcall pre-process-fn acc value)))
                    (ht-set accumulators key new-acc))
                ;; if key is "simple", put as is
                ;; probably only add it if it's not name/namespace
                (asoc-put! item key value)
                (cond ((s-equals? key "NAME")
                       (setq name value))
                      ;; TODO: if namespace is missing from the output, must grab "current" one
                      ;; but only if it's a namespaced resource
                      ((s-equals? key "NAMESPACE")
                       (setq namespace value))))))
          ;; then add all the accumulated goodies
          (dolist (kv (ht-items accumulators))
            (asoc-put! item (car kv) (cadr kv)))
          ;; finally add it to the resulting hashtable
          (ht-set items-ht (cons namespace (cons kind name))
                  ;; post-process just before adding
                  (asoc-map (lambda (k v)
                             (let* ((post-process-fn (ht-get post-process-ht k nil))
                                    (v-new (if post-process-fn (funcall post-process-fn v) v)))
                               ;; update max-column-width
                               (knessy--update-columns-max-width k v-new)
                               (cons k v-new)))
                            item)))
        (forward-line 1)))
    `((:items . ,items-ht)
      (:headers . ((:static . ,header-static)
                   (:repeated . ,header-repeated))))))

;; TODO: this belongs to knessy-process really :)
(cl-defun knessy--shell-exec-parse (cmd buf &optional headers (pre-process-ht (ht)) (post-process-ht (ht)))
  (knessy--shell-exec cmd buf)
  (knessy--parse-table-kubectl-output buf headers pre-process-ht post-process-ht))

;; async read: https://github.com/Silex/docker.el/blob/master/docker-volume.el#L88-L92 , https://github.com/skeeto/emacs-aio/issues/1
;; https://github.com/skeeto/emacs-aio/issues/19
(defun knessy--aio-shell-exec-parse ()
  (let ((promise (aio-promise)))
    (prog1 promise
      (aio-resolve promise (lambda () (knessy--shell-exec-parse "sleep 3 ; kubectl get pods -n kube-system" (get-buffer-create "*test-call-and-parse*")))))))

(aio-defun knessy--aio-display ()
  (let ((result (aio-await (knessy--aio-shell-exec-parse))))
    result))

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
                       (lambda (alist)
                         (asoc-merge alist (ht-get table k)))))))

(comment
 (let ((h (ht (:a '((:k . :v))) (1 '((2 . 3))))))
   (knessy--merge-items
    h
    (ht (:a '((:k2 . :v2)))
        (1 '((3 . 4)))))
   h))

;; TODO: this is where we'd inject the comparators
(defun knessy--make-tablist-format (columns)
  (apply
   #'vector
   (mapcar
    (lambda (column)
      (list column
            (+ 5 (ht-get knessy--columns-max-width column))
            t))
    columns)))

(defun knessy--make-tablist-entries (columns items)
  (mapcar
   (lambda (item)
     (let ((id (car item))
           (alist (cadr item)))
       (list
        id
        (apply
         #'vector
         (mapcar
          (lambda (column)
            (asoc-get alist column))
          columns)))))
   (ht-items items)))

;; TODO: most likely this will be called in the target buffer already
(defun knessy--make-tablist (columns items)
  (setq tabulated-list-format (knessy--make-tablist-format columns))
  (setq tabulated-list-entries (knessy--make-tablist-entries columns items))
  (tabulated-list-init-header)
  (tabulated-list-print))

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
