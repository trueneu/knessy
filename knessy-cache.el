;; -*- lexical-binding: t; -*-

(require 'ht)

;; TODO: add an auto-refresh expired caches by timer capabilities

(defcustom knessy-cache-ttl-default (* 3600 24)
  "Default cache time-to-live."
  :type 'integer
  :group 'knessy)

;; TODO: don't know how to architect this yet...

(comment
 (let ((tbl (ht ('abc 'def))))
   (ht-get* tbl nil)))

(defun knessy--ht-init (table keys)
  "Initializes hashtables all the way down if needed. Returns the last (leaf) hashtable."
  (let ((current-table table))
    (dolist (key keys)
      (unless (ht-get current-table key)
        (ht-set current-table key (ht)))
      ;; descend into madness
      (setq current-table (ht-get current-table key)))
    current-table))

;; TODO: maybe belongs to utils really
(defun knessy--ht-set* (table keys value)
  (let* ((last-key (car (last keys)))
         (butlast-keys (butlast keys))
         (current-table (knessy--ht-init table butlast-keys)))
    (ht-set current-table last-key value)
    value))

;; TODO: actually same as get, just disregard the old value
(defun knessy--cache-set (table keys value &optional ttl)
  (knessy--cache-get table keys (lambda () value) ttl t))

(comment
 (let ((abc 44))
   (let ((abc (if abc abc 456)))
     abc)))

(defun knessy--cache-get (table keys refresh-fn &optional ttl force)
  (knessy--ht-init table keys)
  (let* ((ttl (if ttl ttl knessy-cache-ttl-default))
         (target-table (if keys
                           (apply #'ht-get* table keys)
                         table))
         (now (current-time))
         (refresh-at (ht-get target-table :refresh-at '(0 0 0 0)))
         (data (ht-get target-table :data nil)))
    (if (or force (not data) (time-less-p refresh-at now))
        (progn
          (let ((new-data (funcall refresh-fn)))
            (ht-set target-table :data new-data)
            (ht-set target-table :refresh-at (time-add now ttl))
            new-data))
      data)))

(comment
 (setq trueneu/test-ht (ht))
 (knessy--cache-set
  trueneu/test-ht
  '(abc def ghi)
  456)
 (format-time-string "%H:%M:%S" (time-add (current-time) 0))
 (knessy--get-cached-data-ht trueneu/test-ht
                             '(very-interesting-data)
                             (lambda ()
                               (message "refreshing stuff")
                               (current-time-string))
                             5)
 (knessy--ht-set* trueneu/test-ht '(abc def ghi) 123)
 (last '(abc def ghi) 1)
 trueneu/test-ht
 knessy--kind)


(provide 'knessy-cache)
