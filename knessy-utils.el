;; -*- lexical-binding: t; -*-

; Source - https://stackoverflow.com/a/65697090
; Posted by Vladimir Panteleev
; Retrieved 2026-03-23, License - CC BY-SA 4.0
(defun knessy--insert-into-list (list el n)
  "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil list))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))

(comment
 (let ((l '(1 2 3)))
   (knessy--insert-into-list l 99 (-elem-index 2 l))))

;; TODO: maybe this helper is not needed with generate-new-buffer-name
(defun knessy--get-empty-buffer (buffer-name)
  "Either kill an existing buffer with the name and create a new one, or just create it."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (kill-buffer buf))
    (get-buffer-create buffer-name t)))

(defun knessy--expand-colons (s)
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(provide 'knessy-utils)
