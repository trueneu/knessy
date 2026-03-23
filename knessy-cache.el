;; -*- lexical-binding: t; -*-

(defcustom knessy-cache-ttl-default (* 3600 24)
  "Default cache time-to-live."
  :type 'integer
  :group 'knessy)

;; TODO: don't know how to architect this yet...

(comment
 (let ((tbl (ht ('abc 'def))))
   (ht-get* tbl nil)))

(defun knessy--get-cached (table keys refresh-fn ttl)
  (let ((current-table table))
    (dolist (key keys)
      (unless (ht-get current-table key)
        (ht-set current-table key (ht)))))

  (let* ((target-table (if keys
                           (apply #'ht-get* table keys)
                         table))
         (now (current-time))
         (refresh-at (ht-get target-table :refresh-at '(0 0 0 0)))
         (data (ht-get target-table :data nil)))
    (if (or (not data) (time-less-p refresh-at now))
        (progn
          (let ((new-data (funcall refresh-fn)))
            (ht-set target-table :data new-data)
            (ht-set target-table :refresh-at (time-add now ttl))
            new-data))

      data)))

(comment
 (setq trueneu/test-ht (ht))
 (format-time-string "%H:%M:%S" (time-add (current-time) 0))
 (knessy--get-cached-data-ht trueneu/test-ht
                             '(very-interesting-data)
                             (lambda ()
                               (message "refreshing stuff")
                               (current-time-string))
                             5)
 trueneu/test-ht)

(provide 'knessy-cache)
