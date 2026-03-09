;;; knessy-views.el --- Views (table configurations) for Knessy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pavel Gurkov

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; TODO: test on a cluster with metrics plugin enabled (minikube?)
(require 'ht)
(require 's)
(require 'knessy-options)

(defun knessy--make-convert-and-add-reduce-fn (conversion-fn)
  "Return a reducing function that converts values and adds them up.
CONVERSION-FN is a function of one argument that converts a string
value to a number, e.g. `knessy--convert-cpu-units-millis'.
The returned function takes an accumulator ACC and a value VAL, and
returns ACC plus the result of CONVERSION-FN on VAL.
It is meant to be used as the :reduce-fn of a view's :pre-process
entry, see `knessy--parse-table-kubectl-output'."
  (lambda (acc val)
    (let ((converted (funcall conversion-fn val)))
      (+ acc converted))))

(defcustom knessy-default-column-width
  16
  "Default column width."
  :type 'integer
  :group 'knessy)

;; TODO: make this an alist?
(defcustom knessy-column-widths
  (ht ("NAME" 48)
      ("NAMESPACE" 16)
      ("READY" 5)
      ("RDY" 5)
      ("IP" 16)
      ("STATUS" 12)
      ("RESTARTS" 20)
      ("NODE" 25)
      ("CPU(r)" 30)
      ("MEM(r)" 30)
      ("CPU(l)" 30)
      ("MEM(l)" 30)
      ("HOSTNAMES" 36))
  "Column widths by column name."
  :type 'sexp
  :group 'knessy)

;; TODO: make all structures alists again?.. since they're faster than ht, except items collection maybe
(defcustom knessy-default-view-alist
  '(("pods" . "concise")
    ("csistoragecapacities.storage.k8s.io" . "topology-host"))
  "ALIST of resource resource-type to the view name that's applied by default.
Resource types missing from it get `knessy-default-view-string'.
The value is read whenever a buffer is initialized or its resource type
changes, so updating it takes effect immediately."
  :type '(alist :key-type (string :tag "Resource type")
                :value-type (string :tag "View"))
  :group 'knessy)

(defvar knessy-views)

(defvar knessy--views-by-resource-type nil
  "resource-type->views-list hashtable.
Derived from `knessy-views', see `knessy--recompute-views-by-resource-type'.")

(defun knessy--recompute-views-by-resource-type ()
  "Rebuild `knessy--views-by-resource-type' from `knessy-views'.
Every list starts with `knessy-default-view-string'."
  (let ((res (ht)))
    (dolist (resource-type-view (ht-keys knessy-views))
      (let ((resource-type (car resource-type-view))
            (view (cdr resource-type-view)))
        (unless (ht-contains? res resource-type)
          (ht-set res resource-type '()))
        (ht-update-with! res resource-type (lambda (l) (cons view l)))))
    (dolist (resource-type (ht-keys res))
      (ht-update-with! res resource-type (lambda (l) (cons knessy-default-view-string l))))
    (setq knessy--views-by-resource-type res)))

(defun knessy--set-views (symbol value)
  "Set SYMBOL to VALUE, a `knessy-views' value, and refresh the derived table.
This is the :set function of `knessy-views'."
  (set-default-toplevel-value symbol value)
  (knessy--recompute-views-by-resource-type))

;; (defcustom knessy-default-view
;;   '(:calls . (((:type . :get))))
;;   "An alist depicting the default view"
;;   :type 'sexp
;;   :group 'knessy)

;; TODO: verify that if at least one container doesn't have requests/limits set, pod shows "n/a"
;; TODO: if a container is dead with Init:Error (or most likely isn't running for any other reason) it won't even appear in the top. Hence it will be missing from the output
;; FIXME: add views for deployments, rs, etc -- to propertize DESIRED/CURRENT number of replicas -- this could require tablist entry generation changes
(defcustom knessy-views
  (ht ('("pods" . "top")
       `((:columns . ("NAME" "RDY" "STATUS" "RESTARTS" "CPU(r)" "CPU(l)" "MEM(r)" "MEM(l)" "NODE" "AGE"))
         (:column-rename . ,(ht ("RDY" "READY")))
         (:widths . ,(ht ("NAME" 32)))
         (:calls . (((:type . :get-wide))
                    ((:type . :jsonpath)
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
                     (:post-process-item . ,(lambda (item) nil)))
                    ((:type . :top)
                     (:pre-process . ,(ht ("CPU(cores)" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-cpu-units-millis))))
                                          ("MEMORY(bytes)" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))))))
                     (:post-process . ,(ht ("CPU(cores)" #'knessy--convert-cpu-units-str)
                                           ("MEMORY(bytes)" #'knessy--convert-size-units-str))))))

         (:post-process-item . ,(lambda (item)
                                        (let ((cpu-usage-str (ht-get item "CPU(cores)" "-"))
                                              (cpu-req-str (ht-get item "CPUREQ" "-")))
                                          (ht-set item "CPU(r)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-cpu-units-millis cpu-usage-str)
                                                                  (knessy--convert-cpu-units-millis cpu-req-str))
                                                                 " ("
                                                                 cpu-usage-str
                                                                 "/"
                                                                 cpu-req-str
                                                                 ")")))
                                        (let ((mem-usage-str (ht-get item "MEMORY(bytes)" "-"))
                                              (mem-req-str (ht-get item "MEMREQ" "-")))
                                          (ht-set item "MEM(r)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-size-units-bytes mem-usage-str)
                                                                  (knessy--convert-size-units-bytes mem-req-str))
                                                                 " ("
                                                                 mem-usage-str
                                                                 "/"
                                                                 mem-req-str
                                                                 ")")))
                                        (let ((cpu-usage-str (ht-get item "CPU(cores)" "-"))
                                              (cpu-lim-str (ht-get item "CPULIM" "-")))
                                          (ht-set item "CPU(l)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-cpu-units-millis cpu-usage-str)
                                                                  (knessy--convert-cpu-units-millis cpu-lim-str))
                                                                 " ("
                                                                 cpu-usage-str
                                                                 "/"
                                                                 cpu-lim-str
                                                                 ")")))
                                        (let ((mem-usage-str (ht-get item "MEMORY(bytes)" "-"))
                                              (mem-lim-str (ht-get item "MEMLIM" "-")))
                                          (ht-set item "MEM(l)" (concat
                                                                 (knessy--ratio
                                                                  (knessy--convert-size-units-bytes mem-usage-str)
                                                                  (knessy--convert-size-units-bytes mem-lim-str))
                                                                 " ("
                                                                 mem-usage-str
                                                                 "/"
                                                                 mem-lim-str
                                                                 ")")))))))
      ('("pods" . "concise")
       `((:columns . ("NAME" "RDY" "STATUS" "RESTARTS" "IP" "NODE" "AGE"))
         (:column-rename . ,(ht ("RDY" "READY")))
         (:calls . (((:type . :get-wide))))))

      ('("csistoragecapacities.storage.k8s.io" . "topology-host")
       `((:columns . ("NAME" "CAPACITY" "NODE" "STORAGE CLASS" "CREATED AT"))
         (:widths . ,(ht ("STORAGE CLASS" 24) ("NAME" 24)))
         (:calls . (((:type . :custom-columns)
                     (:spec . "'NAMESPACE:.metadata.namespace,NAME:.metadata.name,CAPACITY:.capacity,NODE:.nodeTopology.matchLabels.hostname,STORAGE CLASS:.storageClassName,CREATED AT:.metadata.creationTimestamp'")
                     (:pre-process . ,(ht ("CAPACITY" `((:reduce-fn . ,(knessy--make-convert-and-add-reduce-fn #'knessy--convert-size-units-bytes))))))
                     (:post-process . ,(ht ("CAPACITY" #'knessy--convert-size-units-str))))))))

      ((cons knessy-all-namespaced-resource-types-string knessy-default-view-string)
       `((:columns . ("NAMESPACE" "NAME" "KIND"))
         (:widths . ,(ht ("KIND" 32)))
         (:calls . (((:type . :all-resources)
                     (:scope . :namespaced)
                     (:spec . "'{range .items[*]}{.kind}|{.metadata.namespace}|{.metadata.name}{\"\\n\"}{end}'")
                     (:headers . ((:static . ("KIND" "NAMESPACE" "NAME")))))))))

      ((cons knessy-linked-resource-type-string knessy-default-view-string)
       `((:columns . ("RESOURCE-TYPE" "NAMESPACE" "NAME"))
         (:widths . ,(ht ("RESOURCE-TYPE" 40)))))

      ((cons knessy-all-cluster-resource-types-string knessy-default-view-string)
       `((:columns . ("NAME" "KIND"))
         (:widths . ,(ht ("KIND" 32)))
         (:calls . (((:type . :all-resources)
                     (:scope . :cluster)
                     (:spec . "'{range .items[*]}{.kind}|{.metadata.name}{\"\\n\"}{end}'")
                     (:headers . ((:static . ("KIND" "NAME"))))))))))


  "The variable defines different queries by resource type."
  :type 'sexp
  :set #'knessy--set-views
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

;;; knessy-views.el ends here
