;; -*- lexical-binding: t; -*-

;; depends on s.el, asoc.el

(defvar knessy--size-units-alist '(("Ki" . Ki)
                                   ("Mi" . Mi)
                                   ("Gi" . Gi)
                                   ("Ti" . Ti)
                                   ("" . B)))

(defvar knessy--size-unit-multipliers-alist `((Ti . ,(* 1024 1024 1024 1024))
                                              (Gi . ,(* 1024 1024 1024))
                                              (Mi . ,(* 1024 1024))
                                              (Ki . 1024)
                                              (B . 1)))

(defvar knessy--time-units-alist '(("s" . s)
                                   ("m" . m)
                                   ("h" . h)
                                   ("d" . d)))
(defvar knessy--time-unit-multipliers-alist `(("s" . 1)
                                              ("m" . 60)
                                              ("h" . ,(* 60 60))
                                              ("d" . ,(* 60 60 24))))

(defun knessy--appropriate-mult (bytes mults)
  "A function returning an appropriate size multiplier cons cell for given
amount of bytes.

BYTES is the number of bytes
MULTS is an alist of multipliers"

  (if (= bytes 0) (cons 'B 1)
    (let* ((current (car mults))
           (name (car current))
           (mult (cdr current))
           (rest (cdr mults)))
      (if (> bytes mult)
          (cons name mult)
        (knessy--appropriate-mult bytes rest)))))

(defun knessy--convert-size-units-bytes (s)
  "Convert a string representing size to bytes integer.

S is the string."
  (if (or (s-equals? "-" s) (s-blank? s))
      0
    (when (string-match (rx bol (group (one-or-more digit)) (group (? (or "Ki" "Mi" "Gi" "Ti")) eol))
                        s)
      (let* ((num (string-to-number (match-string 1 s)))
             (units (asoc-get knessy--size-units-alist (match-string 2 s)))
             (multiplier (asoc-get knessy--size-unit-multipliers-alist units))
             (bytes (* num multiplier)))
        bytes))))

(defun knessy--convert-size-units-str (bytes)
  "Convert bytes into a string with an appropriate multiplier."
  (if (null bytes)
      ""
    (let* ((appropriate-name-mult (knessy--appropriate-mult bytes knessy--size-unit-multipliers-alist))
           (appropriate-name (car appropriate-name-mult))
           (appropriate-mult (cdr appropriate-name-mult))
           (scaled (/ bytes appropriate-mult)))
      (substring-no-properties (format "%.0f%s" scaled appropriate-name)))))

(defun knessy--convert-cpu-units-millis (s)
  "Convert a string representing CPU usage into millicores.

S is the string."
  (if (or (s-equals? "-" s) (s-blank? s))
      0
    (if (s-suffix? "m" s)
        (string-to-number (s-chop-suffix "m" s))
      (* 1000 (string-to-number s)))))

(defun knessy--convert-cpu-units-str (millis)
  "Convert millis into a CPU usage string.

MILLIS is the amount of millicores."
  (if (null millis)
      ""
    (if (> millis 1000)
        (format "%.1f" (/ millis 1000))
      (concat (number-to-string millis) "m"))))

(defun knessy--ratio (x y)
  "Calculate a usage ratio X/Y in percents, return as a string.

If either is null, or y == 0, return \"N/A\""
  (if (or (null y) (zerop y) (null x))
      "N/A"
    (substring-no-properties (format "%.0f%%" (/ x y 0.01)))))

;; TODO: finish this

(defun knessy--convert-time-units-seconds (s)
  "Convert a string representing time to seconds integer.

S is the string."
  (if (or (s-equals? "-" s) (s-blank? s))
      0
    (if (string-match (rx
                       (one-or-more
                        (or
                         (seq
                          (group (one-or-more digit))
                          (group "d"))
                         (seq
                          (group (one-or-more digit))
                          (group "h"))
                         (seq
                          (group (one-or-more digit))
                          (group "m"))
                         (seq
                          (group (one-or-more digit))
                          (group "s")))))
                      s)
        (let ((seconds 0))
          (dotimes (i 4 seconds)
            (let* ((digit-group (1+ (* i 2)))
                   (unit-group (+ 2 (* i 2)))
                   (digit-match (match-string digit-group s))
                   (unit-match (match-string unit-group s)))
              (when digit-match
                (setq seconds (+ seconds
                                 (*
                                  (asoc-get knessy--time-unit-multipliers-alist unit-match)
                                  (string-to-number digit-match))))))))
      -1)))

(comment
 (knessy--convert-time-units-seconds "10d"))
(provide 'knessy-units)

(comment
 (string-match (rx
                (? (group (one-or-more digit))
                   (group "d")))
               "abcd 10d")
 (let ((s "abcd 10d11h"))
   (string-match (rx
                  (one-or-more
                    (or
                     (seq
                      (group (one-or-more digit))
                      (group "d"))
                     (seq
                      (group (one-or-more digit))
                      (group "h")))))
                 s)
   (match-string 0 s)
   (match-string 0 s))
 (rx
                       bol
                       (zero-or-more anychar)
                       (zero-or-more (group (one-or-more digit)) (group "d"))
                       (zero-or-more (group (one-or-more digit)) (group "h"))
                       (zero-or-more (group (one-or-more digit)) (group "m"))
                       (zero-or-more (group (one-or-more digit)) (group "s"))
                       (zero-or-more anychar)
                       eol))
