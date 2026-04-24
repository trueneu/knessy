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
  (knessy--cache-get table keys (lambda () value) ttl t))

(defun knessy--cache-get (table keys refresh-fn &optional ttl force)
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
      (knessy--log 2 (format "Accessing cached data for path:"))
      (knessy--log 2 keys)
      data)))

(provide 'knessy-cache)
