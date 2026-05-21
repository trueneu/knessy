;; -*- lexical-binding: t; -*-

(require 'knessy-utils)

(defun knessy--comparator-make-by (column-num extractor)
  "Return a tabulated-list comparator over COLUMN-NUM.
EXTRACTOR maps the column's string to a number; entries are compared with `<'."
  (lambda (x y)
    (< (funcall extractor (aref (cadr x) column-num))
       (funcall extractor (aref (cadr y) column-num)))))

(defun knessy--comparator-make-time (column-num)
  "Comparator over a time column (e.g. age) at COLUMN-NUM."
  (knessy--comparator-make-by column-num #'knessy--convert-time-units-seconds))

(defun knessy--comparator-make-restarts-count (column-num)
  "Comparator over a restarts column formatted as NUMBER (TIME AGO) at COLUMN-NUM."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (string-to-number (knessy--extract-digits-before-paren s)))))

(defun knessy--comparator-make-ready (column-num)
  "Takes column number containing ready containers column, formatted as READY/TOTAL. Returns a function taking two arguments, x and y, which are vectors with a tabulated-list-mode entry. The function returned is a comparator that extracts READY and TOTAL counts from vectors and compares them one to another. If both are fully ready (READY = TOTAL), then comparison is on TOTAL. If item X is not fully ready but Y is, X is less than Y, and vice versa. If both are not fully ready, comparison is on READY containers."
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
  "Comparator over USAGE in a CPU column formatted as USAGE/TOTAL (RATIO)."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-cpu-units-millis (car (knessy--parse-x-slash-y s))))))

(defun knessy--comparator-make-cpu-ceiling (column-num)
  "Comparator over TOTAL in a CPU column formatted as USAGE/TOTAL (RATIO)."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-cpu-units-millis (cdr (knessy--parse-x-slash-y s))))))

(defun knessy--comparator-make-mem-usage (column-num)
  "Comparator over USAGE in a memory column formatted as USAGE/TOTAL (RATIO)."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-size-units-bytes (car (knessy--parse-x-slash-y s))))))

(defun knessy--comparator-make-mem-ceiling (column-num)
  "Comparator over TOTAL in a memory column formatted as USAGE/TOTAL (RATIO)."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-size-units-bytes (cdr (knessy--parse-x-slash-y s))))))

(defun knessy--comparator-make-percentage (column-num)
  "Comparator over RATIO in a column formatted as USAGE/TOTAL (RATIO)."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (string-to-number (knessy--extract-percentage s)))))

(provide 'knessy-comparators)
