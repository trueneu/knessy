;; -*- lexical-binding: t; -*-
;; requires dash, s.el, asoc.el, el-job (???), aio (???)

(defun knessy--default-sentinel (proc ev)
  (message (format "Received event from process %s: %s"
                   (process-name proc) ev)))

(defun knessy--make-callback-sentinel (f)
  (lambda (proc ev)
    (cond ((s-equals? ev "finished\n")
           (funcall f))
          (t (message (format "Received event from process %s, command \"%s\": %s"
                              (process-name proc)
                              (process-command proc)
                              ev))))))

(defun knessy--make-process-filter-to-buffer (buf)
  (lambda (proc string)
    (princ string buf)))

(defun knessy--expand-list-in-place (orig extension)
  (dolist (elt extension orig)
    (push elt orig)))

(defun knessy--shell-exec-async2 (cmd buf buferr callback)
  (let* ((process (make-process
                   :name "knessy-shell-exec"
                   :command (list "/bin/bash" "-c" cmd)
                   :buffer buf
                   :stderr buferr
                   :filter (knessy--make-process-filter-to-buffer buf)
                   :sentinel (knessy--make-callback-sentinel callback))))))

(comment
 (knessy--shell-exec-async2
  "kubectl get pods -n kube-system"
  (get-buffer-create "bar")
  (get-buffer-create "baz")
  (lambda () (message "I'm done"))))

(defun knessy--shell-exec-async (cmd buf callback)
  (let* ((process (start-process-shell-command "knessy-shell-exec" nil cmd)))
    (set-process-filter process (knessy--make-process-filter-to-buffer buf))
    (set-process-sentinel process (knessy--make-callback-sentinel callback))))

(defun knessy--error-buf ()
  "*Messages*")

(defun knessy--shell-exec (cmd buf)
  (shell-command cmd buf (knessy--error-buf)))

(comment
  (let* ((buf (get-buffer-create "bar")))
    (with-current-buffer buf
      (tabulated-list-mode)
      (setq tabulated-list-format
            '[("NAME" 10 t . ())])
      (setq tabulated-list-entries
            '(('id1 ["abc"])
              ('id2 ["def"])))
      (tabulated-list-init-header))))

(comment
 (aio-defun))

(provide 'knessy-process)
