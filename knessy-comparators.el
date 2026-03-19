;; -*- lexical-binding: t; -*-
(defun knessy--make-comparator-restarts-time (restarts-column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-restarts (aref (cadr x) restarts-column-num))  ; extract the strings
          (y-restarts (aref (cadr y) restarts-column-num)))
      ;; convert and compare
      (<
       (knessy--convert-time-units-seconds x-restarts)
       (knessy--convert-time-units-seconds y-restarts)))))

(provide 'knessy-comparators)
