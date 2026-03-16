;; -*- lexical-binding: t; -*-
;; requires dash, s.el, asoc.el

(defun knessy--default-sentinel (proc ev)
  (message (format "Received event from process %s: %s"
                   (process-name proc) ev)))

(defun knessy--make-callback-sentinel (f)
  (lambda (proc ev)
    (cond ((s-equals? ev "finished\n")
           (funcall f))
          (t (message (format "Received event from process %s: %s"
                              (process-name proc) ev))))))

(defun knessy--make-process-filter-to-buffer (buf)
  (lambda (proc string)
    (princ string buf)))

(defun filter-to-table (proc string)
  (let ((buf (get-buffer-create "bar")))
    (with-current-buffer buf
      (let ((lines (-remove #'s-blank? (s-lines string))))
        (setq tabulated-list-entries
              (knessy--expand-list-in-place
               tabulated-list-entries
               (mapcar (lambda (line)
                        `(nil [,line]))
                       lines))))
      (tabulated-list-print))))

(defun knessy--expand-list-in-place (orig extension)
  (dolist (elt extension orig)
    (push elt orig)))

(defun knessy--shell-exec (cmd buf callback)
  (let* ((process (start-process-shell-command "knessy-shell-exec" nil cmd)))
    (set-process-filter process (knessy--make-process-filter-to-buffer buf))
    (set-process-sentinel process (knessy--make-callback-sentinel callback))))

(comment
 (knessy--shell-exec "kubectl --context general-use-non-production get deployments -n airflow2-test --output name" (get-buffer-create "baz") (lambda () (message "FINISHED!")))
 (knessy--shell-exec "kubectl --context general-use-non-production get deployments -n airflow2-test" (get-buffer-create "baz") (lambda () (message "FINISHED!"))))

(let* ((buf (get-buffer-create "bar")))
  (with-current-buffer buf
    (tabulated-list-mode)
    (setq tabulated-list-format
          '[("NAME" 10 t . ())])
    (setq tabulated-list-entries
          '(('id1 ["abc"])
            ('id2 ["def"])))
    (tabulated-list-init-header)))
