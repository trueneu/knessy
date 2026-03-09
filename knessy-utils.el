;; -*- lexical-binding: t; -*-

(defun knessy--get-empty-buffer (buffer-name)
  "Either kill an existing buffer with the name and create a new one, or just create it."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (kill-buffer buf))
    (get-buffer-create buffer-name)))

(provide 'knessy-utils)
