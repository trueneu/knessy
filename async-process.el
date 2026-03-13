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

  ;; (let* ((buf (get-buffer-create "bar")))
  ;;   (with-current-buffer buf)))

(defun knessy--expand-list-in-place (orig extension)
  (dolist (elt extension orig)
    (push elt orig)))

(defun knessy--shell-exec (cmd buf callback)
  (let* ((process (start-process-shell-command "knessy-shell-exec" nil cmd)))
    (set-process-filter process (knessy--make-process-filter-to-buffer buf))
    (set-process-sentinel process (knessy--make-callback-sentinel callback))))

(defun knessy--parse-table-kubectl-output (buf)
  ;; TODO: we might not even need header heres
  (let ((header)
        (result (asoc-make))
        (items '())
        (items-ht (ht)))
    (with-current-buffer buf
      (goto-char (point-min))

      ;; TODO: this is super-brittle
      (setq header (s-split (rx (>= 2 whitespace))
                            (s-trim (thing-at-point 'line t))))
      (asoc-put! result 'header header)
      (forward-line 1)

      (while (not (eobp))
        (let ((entries (s-split (rx (>= 2 whitespace))
                                (s-trim (thing-at-point 'line t))))
              (item (asoc-make))
              (name)
              (namespace))
          (dolist (pair (-zip-pair header entries))
            (let ((key (car pair))
                  (value (cdr pair)))
              (asoc-put! item key value)
              (cond ((s-equals? key "NAME")
                     (setq name value))
                    ((s-equals? key "NAMESPACE")
                     (setq namespace value)))))
          (ht-set items-ht (cons namespace name) item))

        (forward-line 1)))
    (asoc-put! result 'items items-ht)
    result))

(comment
 (-zip-pair '(1 2 3) '(4 5 6))
 (asoc-get
  (knessy--parse-table-kubectl-output (get-buffer-create "baz"))
  'items))

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
