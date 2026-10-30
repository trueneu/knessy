;; -*- lexical-binding: t; -*-

;; dependencies: dash, asoc, ht, s

(defun knessy--debug-print-ht (table)
  "Print every item of the hash table TABLE with `princ'.
Each item is a list \(KEY VALUE), as returned by `ht-items'.
Output goes to `standard-output'."
  (dolist (item (ht-items table))
    (princ item)))

(defun knessy--update-columns-max-width (table k value)
  "Update the maximum width of the column K in the hash table TABLE.
TABLE maps column names to widths, and is modified in place.
VALUE is a string; the width stored for K becomes the larger of its
current width and the length of VALUE.
If K has no width yet, the length of the name K itself is used as the
starting width."
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
  "Parse buffer BUF with kubectl output into a data structure.
Return nil if BUF is empty.
Otherwise the result looks like this:
  ((:items . HASH-TABLE)
   (:headers . ((:static . \(COL1 COL2 ...))
                (:repeated . \(REPCOL1 ...)))))
The keys of the :items hash table are resource IDs \(NS . \(RT . NAME)).
Its values are hash tables of column name to value, for one resource.
NS is nil if the resource type is not namespaced, see
`knessy--resource-type-namespaced?'.
A NAMESPACE column in the output overrides NS \(blank means nil), a KIND
column overrides RT, and NAME is taken from the NAME column with a
trailing \" (default)\" removed.
Otherwise NS and RT come from `knessy--namespace' and
`knessy--resource-type' as set in the caller's buffer.

BUF is the buffer with info present.
It should only have output from one kubectl call.
Every line is split on two or more whitespace characters, or a \"|\".
Side effects: the values of `knessy--context', `knessy--namespace',
`knessy--resource-type' and `knessy--namespace-current-all?' of the
calling buffer are copied into BUF.
HEADERS is an optional alist of form
  ((:static . \(COL1 COL2 ...))
   (:repeated . \(REPCOL1 REPCOL2 ...)))
If nil, the first line of BUF is taken as a header, split on two or
more whitespace characters, and all of its columns are static.
Static columns appear once in the beginning of the values.
Repeated columns appear as many times as they should at the end, in
looped fashion \(REPCOL1 REPCOL2 REPCOL1 REPCOL2 ...)
This is useful when dealing with complex queries including loops with
unknown number of elements \(e.g. output memory limit for every
container in a pod).
PRE-PROCESS-HT is an optional hash table of column name to an alist
with these optional keys:
  :reduce-fn - function of \(ACC VALUE); if present, all values of the
    column in a row are folded with it, and only the final accumulator
    is stored in the row, under the column name.
  :reduce-acc - initial accumulator, 0 by default.
  :mixin-fn - function of VALUE, called for every value of the column;
    if it returns a cons cell \(KEY . VALUE), that pair is added to the
    row.
The default is an empty hash table, i.e. no pre-processing.
POST-PROCESS-HT is an optional hash table of column name to a function.
Every column specified is modified by applying the function to its value
in every row, after pre-processing.
The default is an empty hash table, i.e. no post-processing.
POST-PROCESS-FN is an optional function called with the row hash table
of each item after all of the above, for side effects on the row.
Nothing is passed on for column widths."
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
          (let* ((line (thing-at-point 'line t))
                 (_ (when (>= knessy--log-level 5)
                      (knessy--log 5 (format "Working on line: %s" line))))
                 (values (s-split (rx (or (>= 2 whitespace) "|"))
                                  (s-trim line)))
                 (item (ht))
                 (name)
                 ;; TODO: here should be the check if resource-type is namespaced or not
                 (resource-type knessy--resource-type)
                 (namespace (if (knessy--resource-type-namespaced? resource-type)
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
                  ;; TODO: this effectively means we can't pre-process NAME, NAMESPACE or KIND
                  (cond ((s-equals? key "NAME")
                         (setq name (s-chop-suffix " (default)" value)))
                        ;; TODO: if namespace is missing from the output, must grab "current" one
                        ;; but only if it's a namespaced resource
                        ((s-equals? key "NAMESPACE")
                         (setq namespace (unless (s-blank? value) value)))
                        ;; the *ALL* resource-type view queries many kinds at once,
                        ;; so the KIND column (not the buffer's resource-type) says
                        ;; what each row actually is
                        ((s-equals? key "KIND")
                         (setq resource-type value))))))

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
  "Merge the items of the hash tables MORE into ITEMS-HT.
ITEMS-HT and each of MORE are hash tables of resource ID to an item,
where an item is a hash table of column name to value.
For every ID in ITEMS-HT, the item of the same ID from each of MORE is
merged into it with `ht-merge'; on equal columns the value from MORE
wins.
IDs that are only present in MORE are ignored.
ITEMS-HT is modified in place and returned."
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
  "Return a `tabulated-list-format' vector for COLUMNS.
COLUMNS is a list of column names.
WIDTHS is a hash table of column name to width; columns missing from it
get `knessy-default-column-width'.
Each element of the returned vector is a list \(NAME WIDTH SORT), where
SORT is a comparator function for the columns AGE, RESTARTS \(time),
RDY \(ready containers), CPU(r), CPU(l), MEM(r) and MEM(l) \(percentage),
and t \(default sorting) for the rest."
  (knessy--log 3 "In knessy--make-tablist-format")
  (let ((column-counter 0))
    (apply
     #'vector
     (mapcar
      (lambda (column)
        (prog1
          (list column
                (ht-get widths column knessy-default-column-width)
                ;; FIXME: we should just make this a hash table
                (cond ((s-equals? "RESTARTS" column)
                       (knessy--comparator-make-time column-counter))
                      ((s-equals? "AGE" column)
                       (knessy--comparator-make-time column-counter))
                      ((s-equals? "RDY" column)
                       (knessy--comparator-make-ready column-counter))
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
  "Return NAME, highlighted if the resource ID is marked.
ID is a resource ID \(NS . \(RT . NAME)).
NAME is the string to display.
If ID has a non-nil entry in the hash table `knessy--marked', return
NAME with the face `dired-marked'.
Otherwise return NAME unchanged."
  (if (ht-get knessy--marked id nil)
      ;; TODO: face should be customizable
      (progn
        (propertize name 'face 'dired-marked))
    name))

(defface knessy-status-running
  '((default . (:inherit success)))
  "Running face")

(defface knessy-status-pending
  '((default . (:inherit warning)))
  "Pending face")

(defface knessy-status-terminating
  '((default . (:inherit error)))
  "Terminating face")

(defface knessy-status-failed
  '((default . (:inherit error)))
  "Failed face")

(defface knessy-ready-all
  '((default . (:inherit default)))
  "All ready face")

(defface knessy-ready-not-all
  '((default . (:inherit error)))
  "Not all ready face")

(defvar knessy-faces
  (ht ("Running" 'knessy-status-running)
      ("Pending" 'knessy-status-pending)
      ("Terminating" 'knessy-status-terminating)
      ("Failed" 'knessy-status-failed)))

(defun knessy--propertize-status (status)
  "Return the string STATUS, propertized with the face for that status.
The face is looked up in the hash table `knessy-faces'.
If there is none, return STATUS unchanged."
  (if-let* ((face (ht-get knessy-faces status nil)))
      (propertize status 'face face)
    status))

(defun knessy--propertize-ready (ready)
  "Return the string READY, propertized according to readiness.
READY is formatted as READY/TOTAL, e.g. \"1/2\".
The READY part gets the face `knessy-ready-not-all' if it is less than
TOTAL, and `knessy-ready-all' otherwise."
  (let* ((x/y (knessy--extract-x-slash-y ready))
         (rdy (string-to-number (car x/y)))
         (total (string-to-number (cdr x/y))))
    (if (< rdy total)
        (s-concat
         (propertize (car x/y) 'face 'knessy-ready-not-all)
         "/" (cdr x/y))
      (s-concat
         (propertize (car x/y) 'face 'knessy-ready-all)
         "/" (cdr x/y)))))

(defcustom knessy-usage-percentage-warning 80
  "TODO"
  :group 'knessy
  :type 'integer)

(defcustom knessy-usage-percentage-critical 90
  "TODO"
  :group 'knessy
  :type 'integer)

(defface knessy-usage-percentage-warning
  '((default . (:inherit warning)))
  "Warning face")

(defface knessy-usage-percentage-critical
  '((default . (:inherit error)))
  "Critical face")

(comment
 (s-split "\s" "abc def"))

(defun knessy--propertize-percentage (s)
  "Return the string S, propertized according to its usage percentage.
S is formatted as \"NN% REST\", e.g. \"85% (850m/1000m)\".
If NN is greater than `knessy-usage-percentage-critical', the
\"NN% \" part gets the face `knessy-usage-percentage-critical'.
Otherwise if it is greater than `knessy-usage-percentage-warning', it
gets the face `knessy-usage-percentage-warning'.
In both cases the result is that part followed by the second
space-separated part of S.
Otherwise S is returned unchanged."
  (let* ((parts (s-split "\s" s))
         (percentage (s-chop-suffix "%" (first parts)))
         (percentage-num (string-to-number percentage)))
    (cond
     ((> percentage-num knessy-usage-percentage-critical)
      (s-concat
       (propertize (s-concat percentage "% ") 'face 'knessy-usage-percentage-critical)
       (second parts)))
     ((> percentage-num knessy-usage-percentage-warning)
      (s-concat
       (propertize (s-concat percentage "% ") 'face 'knessy-usage-percentage-warning)
       (second parts)))
     (t
      s))))

(comment
 (knessy--propertize-status "Running"))

(defvar-local knessy--representation-hidden-entry-ids (ht))
(defvar-local knessy--representation-shadowed-entry-ids (ht))

(defun knessy--propertize-shadow (value)
  "Return the string VALUE with the face `shadow'."
  (propertize value 'face 'shadow))

(defun knessy--make-tablist-entries (columns rename items)
  "Return a list of `tabulated-list-entries' for ITEMS.
COLUMNS is a list of column names, in the order they are displayed.
RENAME is a hash table of displayed column name to the column name used
in the items, for columns that are called differently.
ITEMS is a hash table of resource ID to an item, which is a hash table
of column name to string value.
Each entry is a list \(ID VECTOR) where VECTOR has a string for each of
COLUMNS, \"??\" if the item has no such column.
Name, status, ready and usage columns are propertized.

Side effects: the buffer-local hash tables
`knessy--representation-hidden-entry-ids' and
`knessy--representation-shadowed-entry-ids' are cleared and refilled.
If `knessy--regex' is not blank, every item with no value matching it is
hidden \(left out of the result) if `knessy--regex-hide' is non-nil, and
otherwise shadowed \(all of its values get the face `shadow')."
  (knessy--log 3 "In knessy--make-tablist-entries")
  ;; two-pass, don't know how to make it better
  (ht-clear knessy--representation-hidden-entry-ids)
  (ht-clear knessy--representation-shadowed-entry-ids)

  (unless (s-blank? knessy--regex)
    (mapc
     (lambda (item)
       (let ((id (car item))
             (table (cadr item))
             (match nil))
         (dolist (value (ht-values table))
           (when (string-match-p knessy--regex value)
             (setq match t)))
         (unless match
           (if knessy--regex-hide
               (ht-set knessy--representation-hidden-entry-ids id t)
             (ht-set knessy--representation-shadowed-entry-ids id t)))))
     (ht-items items)))

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
               ((ht-contains? knessy--representation-shadowed-entry-ids id)
                (knessy--propertize-shadow value))
               ((s-equals? "NAME" column)
                (knessy--propertize-name id value))
               ((s-equals? "STATUS" column)
                (knessy--propertize-status value))
               ((s-equals? "RDY" column)
                (knessy--propertize-ready value))
               ((s-equals? "READY" column)
                (knessy--propertize-ready value))
               ((or (s-equals? "CPU(r)" column)
                    (s-equals? "MEM(r)" column)
                    (s-equals? "CPU(l)" column)
                    (s-equals? "MEM(l)" column))
                (knessy--propertize-percentage value))
               (t value))))
          columns)))))
   (-remove
    (lambda (item)
      (let ((id (car item)))
        (ht-contains? knessy--representation-hidden-entry-ids id)))
    (ht-items items))))

;; TODO: most likely this will be called in the target buffer already
(defun knessy--make-tablist (columns rename items widths)
  "Set up and print the table in the current `tabulated-list-mode' buffer.
COLUMNS, RENAME and ITEMS are as in `knessy--make-tablist-entries'.
WIDTHS is a hash table of column name to width, as in
`knessy--make-tablist-format'.
This sets `tabulated-list-format' and `tabulated-list-entries', and
initializes the header.
Unless `knessy--table-remember-pos' is non-nil, the sort key is reset.
The table is printed with `tabulated-list-print', remembering the
position if `knessy--table-remember-pos' was non-nil, which it is set to
afterwards."
  (setq tabulated-list-format (knessy--make-tablist-format columns widths))
  (setq tabulated-list-entries (knessy--make-tablist-entries columns rename items))
  (tabulated-list-init-header)
  (unless knessy--table-remember-pos
    ;; FIXME: if already sorted by NAME, this changes sorting direction
    (setq tabulated-list-sort-key nil))
    ;; (let ((current-prefix-arg -1))
    ;;   (tabulated-list-sort)))
  (tabulated-list-print knessy--table-remember-pos)
  (setq knessy--table-remember-pos t))

;; TODO (pgu, 15.05.2026): maybe this is a bad idea actually.
(defun knessy--mark-tablist-entry (id name-column-num)
  "Highlight the name of the entry ID in the current table.
ID is the entry ID in `tabulated-list-entries'.
NAME-COLUMN-NUM is the index of the name column in the entry's vector.
The name is given the face `dired-marked' in the entry itself, and then
the entry is reprinted at point, replacing the old line.
Point is left on the line before the reprinted entry."
  (let* ((entry (first (asoc-get tabulated-list-entries id nil)))
         (name (aref entry name-column-num))
         (propertized-name (propertize name 'face 'dired-marked)))
    (aset entry name-column-num propertized-name)
    (beginning-of-line)
    (tabulated-list-print-entry id entry)
    (tabulated-list-delete-entry)
    (forward-line -1)))

(defun knessy--unmark-tablist-entry (id name-column-num)
  "Remove the highlighting from the name of the entry ID in the current table.
ID is the entry ID in `tabulated-list-entries'.
NAME-COLUMN-NUM is the index of the name column in the entry's vector.
All text properties of the name are removed in the entry itself, and
then the entry is reprinted at point, replacing the old line.
Point is left on the line before the reprinted entry."
  (let* ((entry (first (asoc-get tabulated-list-entries id nil)))
         (name (aref entry name-column-num))
         (depropertized-name (substring-no-properties name)))
    (aset entry name-column-num depropertized-name)
    (beginning-of-line)
    (tabulated-list-print-entry id entry)
    (tabulated-list-delete-entry)
    (forward-line -1)))

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
