;; -*- lexical-binding: t; -*-

(require 'knessy-utils)

(defun knessy--make-comparator-time (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-time (aref (cadr x) column-num))  ; extract the strings
          (y-time (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-time-units-seconds x-time)
       (knessy--convert-time-units-seconds y-time)))))

;; TODO: these comparators all look awfully alike... generalise them

(defun knessy--make-comparator-restarts-count (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-restarts (aref (cadr x) column-num))  ; extract the strings
          (y-restarts (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (string-to-number (knessy--extract-digits-before-paren x-restarts))
       (string-to-number (knessy--extract-digits-before-paren y-restarts))))))

(defun knessy--make-comparator-ready (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-ready (aref (cadr x) column-num))  ; extract the strings
          (y-ready (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (string-to-number (car (knessy--extract-x-slash-y x-ready)))
       (string-to-number (car (knessy--extract-x-slash-y y-ready)))))))

(defun knessy--make-comparator-cpu-usage (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-cpu (aref (cadr x) column-num))  ; extract the strings
          (y-cpu (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-cpu-units-millis (car (knessy--parse-x-slash-y x-cpu)))
       (knessy--convert-cpu-units-millis (car (knessy--parse-x-slash-y y-cpu)))))))

(defun knessy--make-comparator-cpu-ceiling (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-cpu (aref (cadr x) column-num))  ; extract the strings
          (y-cpu (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-cpu-units-millis (cdr (knessy--parse-x-slash-y x-cpu)))
       (knessy--convert-cpu-units-millis (cdr (knessy--parse-x-slash-y y-cpu)))))))

(defun knessy--make-comparator-mem-usage (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-mem (aref (cadr x) column-num))  ; extract the strings
          (y-mem (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-size-units-bytes (car (knessy--parse-x-slash-y x-mem)))
       (knessy--convert-size-units-bytes (car (knessy--parse-x-slash-y y-mem)))))))

(defun knessy--make-comparator-mem-ceiling (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-mem (aref (cadr x) column-num))  ; extract the strings
          (y-mem (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (knessy--convert-size-units-bytes (cdr (knessy--parse-x-slash-y x-mem)))
       (knessy--convert-size-units-bytes (cdr (knessy--parse-x-slash-y y-mem)))))))

(defun knessy--make-comparator-percentage (column-num)
  (lambda (x y)
    "Returns t if x<y"
    (let ((x-value (aref (cadr x) column-num))  ; extract the strings
          (y-value (aref (cadr y) column-num)))
      ;; convert and compare
      (<
       (string-to-number (knessy--extract-percentage x-value))
       (string-to-number (knessy--extract-percentage y-value))))))

(provide 'knessy-comparators)
