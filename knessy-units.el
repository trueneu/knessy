;; -*- lexical-binding: t; -*-

;; depends on s.el, asoc.el

(defvar knessy--size-units-alist '(("Ki" . Ki)
                                   ("K" . K)
                                   ("Mi" . Mi)
                                   ("M" . M)
                                   ("Gi" . Gi)
                                   ("G" . G)
                                   ("Ti" . Ti)
                                   ("T" . T)
                                   ("" . B)))

(defvar knessy--size-unit-multipliers-alist `((Ti . ,(* 1024 1024 1024 1024))
                                              (T . ,(* 1000 1000 1000 1000))
                                              (Gi . ,(* 1024 1024 1024))
                                              (G . ,(* 1000 1000 1000))
                                              (Mi . ,(* 1024 1024))
                                              (M . ,(* 1000 1000))
                                              (Ki . 1024)
                                              (K . 1000)
                                              (B . 1)))

(defvar knessy--size-unit-pow2-multipliers-alist `((Ti . ,(* 1024 1024 1024 1024))
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
  "Return the multiplier cons cell suitable for displaying BYTES.
BYTES is a number of bytes.
MULTS is an alist of \(NAME . MULTIPLIER) pairs, ordered from the
largest multiplier to the smallest, e.g.
`knessy--size-unit-pow2-multipliers-alist'.
If BYTES is 0, return \(B . 1).
Otherwise return the first pair in MULTS whose multiplier is strictly
less than BYTES.
MULTS should end with a small multiplier such as \(B . 1), since the
list is walked recursively until a match is found."

  (if (= bytes 0) (cons 'B 1)
    (let* ((current (car mults))
           (name (car current))
           (mult (cdr current))
           (rest (cdr mults)))
      (if (> bytes mult)
          (cons name mult)
        (knessy--appropriate-mult bytes rest)))))

(defun knessy--convert-size-units-bytes (s)
  "Convert the size string S to a number of bytes.
S looks like \"500M\", \"400Mi\" or \"12\", that is a number optionally
followed by one of K, M, G, T, Ki, Mi, Gi, Ti.
Decimal suffixes (K, M, G, T) are powers of 1000, binary suffixes
\(Ki, Mi, Gi, Ti) are powers of 1024, and no suffix means bytes.
If S is \"-\" or blank, return 0.
If S does not look like a size, return nil.
Use `knessy--size-units-alist' and `knessy--size-unit-multipliers-alist'."
  (if (or (s-equals? "-" s) (s-blank? s))
      0
    (when (string-match (rx bol (group (one-or-more (or digit ".")) (group (? (or "Ki" "Mi" "Gi" "Ti" "K" "M" "G" "T")) eol)))
                        s)
      (let* ((num (string-to-number (match-string 1 s)))
             (units (asoc-get knessy--size-units-alist (match-string 2 s)))
             (multiplier (asoc-get knessy--size-unit-multipliers-alist units))
             (bytes (* num multiplier)))
        bytes))))

(comment
 (knessy--convert-size-units-bytes "500M"))

(defun knessy--convert-size-units-str (bytes)
  "Convert BYTES into a string with an appropriate binary unit.
BYTES is a number of bytes, or nil.
If BYTES is nil, return the empty string.
Otherwise pick a unit among Ti, Gi, Mi, Ki, B with
`knessy--appropriate-mult' and return the scaled value with three
decimals followed by the unit, e.g. \"1.500Mi\".
The result has no text properties."
  (if (null bytes)
      ""
    (let* ((appropriate-name-mult (knessy--appropriate-mult bytes knessy--size-unit-pow2-multipliers-alist))
           (appropriate-name (car appropriate-name-mult))
           (appropriate-mult (cdr appropriate-name-mult))
           (scaled (/ (float bytes) appropriate-mult)))
      (substring-no-properties (format "%.3f%s" scaled appropriate-name)))))

(defun knessy--convert-cpu-units-millis (s)
  "Convert the CPU usage string S into millicores.
S is either a number of millicores with an \"m\" suffix, e.g. \"250m\",
or a number of cores, e.g. \"0.5\" or \"2\".
If S is \"-\" or blank, return 0.
Return a number of millicores."
  (if (or (s-equals? "-" s) (s-blank? s))
      0
    (if (s-suffix? "m" s)
        (string-to-number (s-chop-suffix "m" s))
      (* 1000 (string-to-number s)))))

(defun knessy--convert-cpu-units-str (millis)
  "Convert MILLIS into a CPU usage string.
MILLIS is an amount of millicores, or nil.
If MILLIS is nil, return the empty string.
If MILLIS is greater than 1000, return cores with three decimals and
no suffix, e.g. \"1.500\".
Otherwise return millicores with an \"m\" suffix, e.g. \"250m\"."
  (if (null millis)
      ""
    (if (> millis 1000)
        (format "%.3f" (/ (float millis) 1000))
      (concat (number-to-string millis) "m"))))

(defun knessy--ratio (x y)
  "Return the usage ratio X/Y in percents, as a string such as \"42%\".
X and Y are numbers.
If X or Y is nil, or Y is zero, return \"?\".
The result has no text properties."
  (if (or (null y) (zerop y) (null x))
      "?"
    (substring-no-properties (format "%.0f%%" (/ (float x) y 0.01)))))

(defun knessy--convert-time-units-seconds (s)
  "Convert the duration string S to a number of seconds.
S is a sequence of components with d, h, m, s suffixes, e.g. \"3d4h\",
\"5m10s\" or \"42s\".
Every component found is multiplied by its unit and added up.
If S is \"-\" or blank, return 0.
If S contains no such component, return a large constant,
3600 * 24 * 7 * 365 * 10 seconds."
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
      (* 3600 24 7 365 10))))

(defun knessy--extract-x-slash-y (s)
  "Extract the X and Y parts of the first X/Y found in the string S.
X and Y are runs of characters other than parentheses, and are not
trimmed.
Return a cons cell \(X . Y) of strings.
If S has no such part, return \(\"0\" . \"0\")."
  (if (string-match (rx (group (one-or-more (not (any "()"))))
                        "/"
                        (group (one-or-more (not (any "()")))))
                    s)
      (cons (match-string 1 s) (match-string 2 s))
    (cons "0" "0")))

(defun knessy--extract-percentage (s)
  "Extract the first percentage from the string S.
Return the digits before the first \"%\" as a string, without the sign.
If S has no such part, return \"-1\"."
  (if (string-match (rx (group (one-or-more digit))
                        "%")

                    s)
      (match-string 1 s)
    "-1"))

(defun knessy--extract-digits-before-paren (s)
  "Extract the digits that precede an opening parenthesis in the string S.
The digits must be followed by at least one blank and then \"(\",
as in \"3 (2h ago)\".
Return the digits as a string.
If S has no such part, return \"0\"."
  (if (string-match (rx (group (one-or-more digit))
                        (one-or-more blank) "(")

                    s)
      (match-string 1 s)
    "0"))

(comment
 (knessy--extract-percentage "? (1/1)"))


(comment
 (knessy--extract-x-slash-y "2% (10m/100m)"))

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
