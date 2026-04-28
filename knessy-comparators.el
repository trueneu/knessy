;; -*- lexical-binding: t; -*-

(require 'knessy-utils)

(defun knessy--comparator-make-time (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-time (aref (cadr x) column-num))  ; extract the strings
          (y-time (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-time-units-seconds x-time)
       (knessy--convert-time-units-seconds y-time)))))

;; TODO: these comparators all look awfully alike... generalise them

(defun knessy--comparator-make-restarts-count (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-restarts (aref (cadr x) column-num))  ; extract the strings
          (y-restarts (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (string-to-number (knessy--extract-digits-before-paren x-restarts))
       (string-to-number (knessy--extract-digits-before-paren y-restarts))))))

(defun knessy--comparator-make-ready (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let* ((x-ready (aref (cadr x) column-num))  ; extract the strings
           (y-ready (aref (cadr y) column-num))
           (x-rdy-total (knessy--extract-x-slash-y x-ready))
           (y-rdy-total (knessy--extract-x-slash-y y-ready))
           (x-rdy (string-to-number (car x-rdy-total)))
           (x-total (string-to-number (cdr x-rdy-total)))
           (y-rdy (string-to-number (car y-rdy-total)))
           (y-total (string-to-number (cdr y-rdy-total))))
      ;; convert and compare
      (cond ((and (= x-rdy x-total)     ; both are fully ready
                  (= y-rdy y-total))
             (< x-total y-total))       ; compare by total
            ((and (< x-rdy x-total)     ; if x is not fully ready but y is
                  (= y-rdy y-total))
             t)                         ; it's less
            ((and (= x-rdy x-total)     ; if y is not fully ready but x is
                  (< y-rdy y-total))
             nil)                       ; it's greater
            (t                          ; if both are not fully ready
             (< x-rdy y-rdy))))))       ; compare by number of ready containers


(defun knessy--comparator-make-cpu-usage (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-cpu (aref (cadr x) column-num))  ; extract the strings
          (y-cpu (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-cpu-units-millis (car (knessy--parse-x-slash-y x-cpu)))
       (knessy--convert-cpu-units-millis (car (knessy--parse-x-slash-y y-cpu)))))))

(defun knessy--comparator-make-cpu-ceiling (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-cpu (aref (cadr x) column-num))  ; extract the strings
          (y-cpu (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-cpu-units-millis (cdr (knessy--parse-x-slash-y x-cpu)))
       (knessy--convert-cpu-units-millis (cdr (knessy--parse-x-slash-y y-cpu)))))))

(defun knessy--comparator-make-mem-usage (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-mem (aref (cadr x) column-num))  ; extract the strings
          (y-mem (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-size-units-bytes (car (knessy--parse-x-slash-y x-mem)))
       (knessy--convert-size-units-bytes (car (knessy--parse-x-slash-y y-mem)))))))

(defun knessy--comparator-make-mem-ceiling (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-mem (aref (cadr x) column-num))  ; extract the strings
          (y-mem (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-size-units-bytes (cdr (knessy--parse-x-slash-y x-mem)))
       (knessy--convert-size-units-bytes (cdr (knessy--parse-x-slash-y y-mem)))))))

(defun knessy--comparator-make-percentage (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-value (aref (cadr x) column-num))  ; extract the strings
          (y-value (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (string-to-number (knessy--extract-percentage x-value))
       (string-to-number (knessy--extract-percentage y-value))))))

(provide 'knessy-comparators)
