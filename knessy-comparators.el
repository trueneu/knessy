;;; knessy-comparators.el --- Column comparators for Knessy tables  -*- lexical-binding: t; -*-

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

(require 'knessy-utils)

(defun knessy--comparator-make-by (column-num extractor)
  "Return a tabulated-list comparator over the column COLUMN-NUM.
COLUMN-NUM is the index of the column in the entry's vector.
EXTRACTOR maps the column's string to a number; entries are compared with `<'.
The returned function takes two tabulated-list entries X and Y, each of
form \(ID VECTOR), and returns non-nil if X sorts before Y."
  (lambda (x y)
    (< (funcall extractor (aref (cadr x) column-num))
       (funcall extractor (aref (cadr y) column-num)))))

(defun knessy--comparator-make-time (column-num)
  "Return a comparator over a time column \(e.g. age) at COLUMN-NUM.
Values are converted with `knessy--convert-time-units-seconds'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by column-num #'knessy--convert-time-units-seconds))

(defun knessy--comparator-make-restarts-count (column-num)
  "Return a comparator over a restarts column at COLUMN-NUM.
The column is formatted as NUMBER \(TIME AGO), and entries are compared
by NUMBER, extracted with `knessy--extract-digits-before-paren'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (string-to-number (knessy--extract-digits-before-paren s)))))

(defun knessy--comparator-make-ready (column-num)
  "Return a comparator over a ready column at COLUMN-NUM.
The column holds ready containers formatted as READY/TOTAL.
The returned function takes two arguments X and Y, which are
tabulated-list entries \(ID VECTOR), and returns non-nil if X sorts
before Y.
READY and TOTAL are extracted with `knessy--extract-x-slash-y'.
If both are fully ready \(READY = TOTAL), comparison is on TOTAL.
If X is not fully ready but Y is, X is less than Y, and vice versa.
If both are not fully ready, comparison is on READY."
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
  "Return a comparator over USAGE in a CPU column at COLUMN-NUM.
The column is formatted as USAGE/TOTAL \(RATIO).
USAGE is converted to millicores with `knessy--convert-cpu-units-millis'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-cpu-units-millis (car (knessy--extract-x-slash-y s))))))

(defun knessy--comparator-make-cpu-ceiling (column-num)
  "Return a comparator over TOTAL in a CPU column at COLUMN-NUM.
The column is formatted as USAGE/TOTAL \(RATIO).
TOTAL is converted to millicores with `knessy--convert-cpu-units-millis'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-cpu-units-millis (cdr (knessy--extract-x-slash-y s))))))

(defun knessy--comparator-make-mem-usage (column-num)
  "Return a comparator over USAGE in a memory column at COLUMN-NUM.
The column is formatted as USAGE/TOTAL \(RATIO).
USAGE is converted to bytes with `knessy--convert-size-units-bytes'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-size-units-bytes (car (knessy--extract-x-slash-y s))))))

(defun knessy--comparator-make-mem-ceiling (column-num)
  "Return a comparator over TOTAL in a memory column at COLUMN-NUM.
The column is formatted as USAGE/TOTAL \(RATIO).
TOTAL is converted to bytes with `knessy--convert-size-units-bytes'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (knessy--convert-size-units-bytes (cdr (knessy--extract-x-slash-y s))))))

(defun knessy--comparator-make-size (column-num)
  "Return a comparator over a size column \(e.g. capacity) at COLUMN-NUM.
Values such as \"1.500Mi\" are converted with
`knessy--convert-size-units-bytes'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by column-num #'knessy--convert-size-units-bytes))

(defun knessy--comparator-make-percentage (column-num)
  "Return a comparator over RATIO in a column at COLUMN-NUM.
The column is formatted as USAGE/TOTAL \(RATIO), and RATIO is extracted
with `knessy--extract-percentage'.
See `knessy--comparator-make-by' for the returned function."
  (knessy--comparator-make-by
   column-num
   (lambda (s) (string-to-number (knessy--extract-percentage s)))))

(provide 'knessy-comparators)

;;; knessy-comparators.el ends here
