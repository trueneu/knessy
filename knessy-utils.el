;; -*- lexical-binding: t; -*-

; Source - https://stackoverflow.com/a/65697090
; Posted by Vladimir Panteleev
; Retrieved 2026-03-23, License - CC BY-SA 4.0
(defun knessy--utils-insert-into-list (list el n)
  "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil list))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))

;; TODO: maybe this helper is not needed with generate-new-buffer-name
(defun knessy--utils-make-buffer (buffer-name)
  "Either kill an existing buffer with the name and create a new one, or just create it."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (kill-buffer buf))
    (get-buffer-create buffer-name t)))

(defun knessy--utils-filename-expand-colons (s)
  "Expands file name for multiple files divided by colons."
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(defun knessy--utils-set (l)
  "Makes a hashset from the list L"
  (let ((res (ht)))
    (dolist (entry l res)
      (ht-set res entry t))))

(defun knessy--utils-ht-merge-duplicates-to-sets (&rest tables)
  (let ((res-table (ht)))
    (dolist (table tables res-table)
      (dolist (item (ht-items table))
        (let ((key (car item))
              (value (cadr item)))
          (if (not (ht-contains? res-table key))
              (ht-set res-table key (ht (value t)))
            (ht-set (ht-get res-table key) value t)))))))

(defun knessy--utils-ht-init (table keys)
  "Initializes hashtables all the way down if needed. Returns the last (leaf) hashtable."
  (let ((current-table table))
    (dolist (key keys)
      (unless (ht-get current-table key)
        (ht-set current-table key (ht)))
      ;; descend into madness
      (setq current-table (ht-get current-table key)))
    current-table))

(defun knessy--utils-ht-set* (table keys value)
  "Sets an aribitrarily nested key to value, creating hashtables along the way if don't exist."
  (let* ((last-key (car (last keys)))
         (butlast-keys (butlast keys))
         (current-table (knessy--utils-ht-init table butlast-keys)))
    (ht-set current-table last-key value)
    value))

;; TODO: can generalize this to readlines and save to a list
(defun knessy--utils-read-buffer (buf &optional no-kill)
  (let (new-cache)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((context (s-trim (thing-at-point 'line))))
          (push context new-cache))
        (forward-line 1))
      (unless no-kill
        (kill-buffer)))
    new-cache))

(defun knessy--utils-kubectl-buffer-name (what &optional omit-context? omit-namespace? omit-kind? stderr?)
  (s-concat
   "*"
   knessy-base-buffer-name
   (if omit-context?
       ""
     (s-concat "-" knessy--context))
   (if omit-namespace?
       ""
     (s-concat "-" knessy--namespace))
   (if omit-kind?
       ""
     (s-concat "-" knessy--kind))
   "-"
   what
   (if stderr?
       "-stderr"
     "")
   "*"))

(provide 'knessy-utils)
