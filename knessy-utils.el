;;; knessy-utils.el --- Helper functions for Knessy  -*- lexical-binding: t; -*-

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

(require 'ht)
(require 's)
(require 'asoc)

; Source - https://stackoverflow.com/a/65697090
; Posted by Vladimir Panteleev
; Retrieved 2026-03-23, License - CC BY-SA 4.0
(defun knessy--utils-insert-into-list (list el n)
  "Insert into list LIST an element EL at index N.
If N is 0, EL is inserted before the first element.
N must not be greater than the length of LIST.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil list))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))

(defun knessy--utils-make-buffer (buffer-name)
  "Create and return a fresh buffer named BUFFER-NAME.
If a buffer with that name already exists, kill it first.
The new buffer is created with buffer hooks inhibited."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (kill-buffer buf))
    (get-buffer-create buffer-name t)))

(defun knessy--utils-filename-expand-colons (s)
  "Expand every file name in the colon-separated list S.
S is a string like \"a/b:c/d\", for example a KUBECONFIG value.
Each part is passed to `expand-file-name', relative to the current
`default-directory'.
Return the expanded names joined back with colons."
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(defun knessy--utils-set (l)
  "Make a hash set from the list L.
Return a new hash table, created with `ht', which has every element
of L as a key and t as the value."
  (let ((res (ht)))
    (dolist (entry l res)
      (ht-set res entry t))))

(defun knessy--utils-set-intersection (ht1 ht2)
  "Compute the intersection of the hash sets HT1 and HT2.
Both are hash tables used as sets, i.e. only their keys matter.
Return a list \(not a hash table) of the keys present in both.
The order of the list is unspecified."
  (interactive)
  (let* ((ht1-smaller? (< (ht-size ht1) (ht-size ht2)))
         (smaller-set (if ht1-smaller? ht1 ht2))
         (larger-set  (if ht1-smaller? ht2 ht1))
         (intersection '()))
    (dolist (key (ht-keys smaller-set) intersection)
      (when (ht-contains? larger-set key)
        (push key intersection)))))

(defun knessy--utils-ht-merge-duplicates-to-sets (&rest tables)
  "Merge hash tables TABLES, collecting the values of equal keys into sets.
Each of TABLES is a hash table.
Return a new hash table which maps every key seen in any of TABLES to a
hash set \(value -> t) of all the values the key had."
  (let ((res-table (ht)))
    (dolist (table tables res-table)
      (dolist (item (ht-items table))
        (let ((key (car item))
              (value (cadr item)))
          (if (not (ht-contains? res-table key))
              (ht-set res-table key (ht (value t)))
            (ht-set (ht-get res-table key) value t)))))))

(defun knessy--utils-ht-init (table keys)
  "Make sure the nested hash tables addressed by KEYS exist in TABLE.
TABLE is a hash table, KEYS is a list of keys to descend through.
For every key, if there is no non-nil value in the current table, set
it to a new empty hash table, then descend into it.
TABLE is modified in place.
Return the last \(leaf) hash table."
  (let ((current-table table))
    (dolist (key keys)
      (unless (ht-get current-table key)
        (ht-set current-table key (ht)))
      ;; descend into madness
      (setq current-table (ht-get current-table key)))
    current-table))

(defun knessy--utils-ht-set* (table keys value)
  "Set an arbitrarily nested key in TABLE to VALUE.
TABLE is a hash table.
KEYS is a non-empty list of keys, the last one being set to VALUE in the
table addressed by the previous ones.
Missing hash tables along the way are created with
`knessy--utils-ht-init'.
Return VALUE."
  (let* ((last-key (car (last keys)))
         (butlast-keys (butlast keys))
         (current-table (knessy--utils-ht-init table butlast-keys)))
    (ht-set current-table last-key value)
    value))

;; TODO: can generalize this to readlines and save to a list
(defun knessy--utils-read-buffer (buf &optional no-kill)
  "Read the lines of buffer BUF into a list of trimmed strings.
Every line of BUF, blank lines included, is trimmed with `s-trim'.
Point in BUF is moved.
Kill BUF afterwards, unless NO-KILL is non-nil.
Return the list of lines in reverse order, last line first."
  (let (new-cache)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((thing (thing-at-point 'line))
                    (context (s-trim thing)))
          (push context new-cache))
        (forward-line 1))
      (unless no-kill
        (kill-buffer)))
    new-cache))

(defun knessy--utils-kubectl-buffer-name (what &optional omit-context? omit-namespace? omit-resource-type? stderr?)
  "Return a buffer name for a kubectl-related buffer, as a string.
WHAT is a string describing the buffer's purpose.
The name looks like \"*BASE-CONTEXT-NAMESPACE-RESOURCE-TYPE-WHAT*\",
built from `knessy-base-buffer-name', `knessy--context',
`knessy--namespace' and `knessy--resource-type'.
If OMIT-CONTEXT?, OMIT-NAMESPACE? or OMIT-RESOURCE-TYPE? is non-nil,
the corresponding part is left out.
If STDERR? is non-nil, the name of the stderr buffer is returned,
i.e. \"-stderr\" is appended after WHAT."
  (s-concat
   "*"
   knessy-base-buffer-name
   (if omit-context?
       ""
     (s-concat "-" knessy--context))
   (if omit-namespace?
       ""
     (s-concat "-" knessy--namespace))
   (if omit-resource-type?
       ""
     (s-concat "-" knessy--resource-type))
   "-"
   what
   (if stderr?
       "-stderr"
     "")
   "*"))

;; Resource IDs are dotted pairs of the form (NS . (RT . NAME)),
;; constructed in knessy-representation.el. NS is nil for cluster-scoped
;; resources. Always go through these accessors instead of car/cadr/cddr.
(defun knessy--id-ns (id)
  "Return the namespace of the resource ID.
ID is a dotted pair \(NS . \(RT . NAME)).
NS is nil for cluster-scoped resources."
  (car id))
(defun knessy--id-rt (id)
  "Return the resource type of the resource ID.
ID is a dotted pair \(NS . \(RT . NAME))."
  (cadr id))
(defun knessy--id-name (id)
  "Return the name of the resource ID.
ID is a dotted pair \(NS . \(RT . NAME))."
  (cddr id))

(defun knessy--utils-fresh-kubectl-buffer (what &optional omit-context? omit-namespace? omit-resource-type? stderr?)
  "Create a fresh kubectl buffer with a unique generated name, and return it.
WHAT, OMIT-CONTEXT?, OMIT-NAMESPACE?, OMIT-RESOURCE-TYPE? and STDERR?
are forwarded to `knessy--utils-kubectl-buffer-name'.
The name is made unique with `generate-new-buffer-name'."
  (knessy--utils-make-buffer
   (generate-new-buffer-name
    (knessy--utils-kubectl-buffer-name
     what omit-context? omit-namespace? omit-resource-type? stderr?))))

(defun knessy--utils-alist->str-= (alist)
  "Convert ALIST into a string of comma-separated KEY=VALUE pairs.
ALIST is an association list, e.g. of Kubernetes labels.
Keys and values are formatted with `%s'.
Return the empty string if ALIST is nil."
  (s-join
   ","
   (asoc-map
    (lambda (k v)
      (format "%s=%s" k v))
    alist)))

(provide 'knessy-utils)

;;; knessy-utils.el ends here
