;; -*- lexical-binding: t; -*-

;; TODO: maybe this helper is not needed with generate-new-buffer-name
(defun knessy--get-empty-buffer (buffer-name)
  "Either kill an existing buffer with the name and create a new one, or just create it."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (kill-buffer buf))
    (get-buffer-create buffer-name)))

(defun knessy--expand-colons (s)
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(provide 'knessy-utils)
