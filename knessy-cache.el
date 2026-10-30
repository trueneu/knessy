;; -*- lexical-binding: t; -*-

(require 'ht)

;; TODO: add an auto-refresh expired caches by timer capabilities

(defcustom knessy-cache-ttl-default (* 3600 24)
  "Default cache time-to-live."
  :type 'integer
  :group 'knessy)

(defvar knessy--cache
  (ht)
  "Caches everything.")

(defun knessy--cache-set (table keys value &optional ttl)
  "Store VALUE in the cache TABLE under the path KEYS, and return it.
TABLE is a nested hash table, e.g. `knessy--cache'.
KEYS is a list of keys that lead to the entry; missing tables are created.
TTL is optional, in seconds; if nil, `knessy-cache-ttl-default' is used.
The entry is overwritten even if it is still fresh, and its expiration time
is restarted.
See `knessy--cache-get'."
  (knessy--cache-get table keys (lambda () value) ttl t))

(defun knessy--cache-fresh-p (table keys)
  "Return non-nil if TABLE has unexpired data at KEYS.
TABLE is a nested hash table, e.g. `knessy--cache', and KEYS is the list of
keys leading to the entry.
Unlike `knessy--cache-get', never creates entries or refreshes anything."
  (let* ((target-table (let ((tbl table))
                         (dolist (key keys tbl)
                           (setq tbl (and (hash-table-p tbl) (ht-get tbl key))))))
         (refresh-at (and (hash-table-p target-table) (ht-get target-table :refresh-at))))
    (and (hash-table-p target-table)
         (ht-get target-table :data)
         refresh-at
         (time-less-p (current-time) refresh-at))))

(defun knessy--cache-get (table keys refresh-fn &optional ttl force)
  "Return the data cached in TABLE at the path KEYS, refreshing it if needed.
TABLE is a nested hash table, e.g. `knessy--cache'.
KEYS is a list of keys leading to the entry; missing tables along the path
are created.
REFRESH-FN is a function of no arguments that returns the new data.
It is called if the entry has no data or has expired, or if FORCE is
non-nil.
TTL is optional, in seconds; if nil, `knessy-cache-ttl-default' is used.
It is applied when the entry is refreshed.
FORCE is optional; if non-nil, REFRESH-FN is called unconditionally.
The result of REFRESH-FN is stored in the entry together with its expiration
time, and is returned; otherwise the cached data is returned.
Cache accesses and refreshes are logged."
  (knessy--utils-ht-init table keys)
  (let* ((ttl (if ttl ttl knessy-cache-ttl-default))
         (target-table (if keys
                           (apply #'ht-get* table keys)
                         table))
         (now (current-time))
         (refresh-at (ht-get target-table :refresh-at '(0 0 0 0)))
         (data (ht-get target-table :data nil)))
    (if (or force (not data) (time-less-p refresh-at now))
        (progn
          (knessy--log 2 (format "Refreshing cache for path:"))
          (knessy--log 2 keys)
          (let ((new-data (funcall refresh-fn)))
            (ht-set target-table :data new-data)
            (ht-set target-table :refresh-at (time-add now ttl))
            new-data))
      (knessy--log 4 (format "Accessing cached data for path:"))
      (knessy--log 4 keys)
      data)))

(comment
 (knessy--cache-resource-types-populate-async))

(provide 'knessy-cache)
