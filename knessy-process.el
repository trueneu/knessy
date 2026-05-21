;; -*- lexical-binding: t; -*-
;; requires dash, s.el, asoc.el, el-job (???), aio (???)

(defvar knessy-shell-always-kill-error-buffers nil
  "If t, errored-out buffers are always killed, if nil only when there's no errors.")
(defvar knessy-shell-kill-success-buffers t
  "If t, buffers with command output are killed")

(defun knessy--make-callback-sentinel (f buf buferr)
  (lambda (proc ev)
    (cond ((s-equals? ev "finished\n")
           (prog1
             (funcall f)
             (kill-buffer buferr)))
          (t
           (let ((msg (if knessy-shell-always-kill-error-buffers
                          (progn
                            (kill-buffer buferr)
                            (format "Received event from process %s, command \"%s\": %s"
                                    (process-name proc)
                                    (process-command proc)
                                    ev))
                        (format "Received event from process %s, command \"%s\": %s; see buffer \"%s\" for details"
                            (process-name proc)
                            (process-command proc)
                            ev
                            (buffer-name buferr)))))
               (message msg))))))

(defun knessy--make-process-filter-to-buffer (buf)
  (lambda (proc string)
    (princ string buf)))

(defface knessy-log-face1
  '((default . (:foreground "black" :weight bold)))
  "Log face 1")

(defface knessy-log-face2
  '((default . (:foreground "teal" :weight bold)))
  "Log face 2")

(defface knessy-log-face3
  '((default . (:foreground "blue" :weight bold)))
  "Log face 3")

(defface knessy-log-face4
  '((default . (:inherit font-lock-function-name-face :weight bold)))
  "Log face 4")

(defvar knessy--log-faces '(knessy-log-face1
                            knessy-log-face2
                            knessy-log-face3
                            knessy-log-face4))

(defvar knessy--log-face-counter 0)

(defface knessy-ts-face
  '((default . (:inherit default :weight bold)))
  "Timestamp face")

(defun knessy--next-log-face ()
  "Pop the next face from `knessy--log-faces', round-robin."
  (prog1 (nth knessy--log-face-counter knessy--log-faces)
    (setq knessy--log-face-counter
          (mod (1+ knessy--log-face-counter) (length knessy--log-faces)))))

(defun knessy--make-line-filter (buf line-fn)
  "Return a process filter that buffers OUTPUT into complete lines.
For each complete line, calls LINE-FN with that line in BUF, with point at
`point-max' and `inhibit-read-only' bound. Auto-follows point if it was at
end-of-buffer before output arrived."
  (lambda (proc output)
    (when (buffer-live-p buf)
      (let* ((pending (process-get proc 'knessy--line-buffer))
             (combined (concat pending output))
             (lines (split-string combined "\n"))
             (incomplete (car (last lines)))
             (complete (butlast lines)))
        (process-put proc 'knessy--line-buffer incomplete)
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (moving (= (point) (point-max))))
            (save-excursion
              (goto-char (point-max))
              (dolist (line complete) (funcall line-fn line)))
            (when moving (goto-char (point-max)))))))))

;; TODO (pgu, 19.05.2026): make another filter, with tabulated-list-mode and tablist-print-entry
(defun knessy--make-process-filter-logs (buf &optional name print-prefix? print-prefix-width)
  (let ((face (knessy--next-log-face)))
    (knessy--make-line-filter
     buf
     (lambda (line)
       (when print-prefix?
         (insert
          (propertize (s-pad-right print-prefix-width " " (format "[%s]" name))
                      'face face)))
       (insert line "\n")))))

(comment
 (let* ((kubectl-log-prefix-regex (rx bol "[" (group (one-or-more (| letter "." "-"))) "/" (group (one-or-more (| letter digit "-"))) "/" (group (one-or-more (| letter digit "-"))) "]" (char whitespace) (group (one-or-more (| digit "-" ":" "T" "." "Z"))) (char whitespace) (group (zero-or-more anychar))))
        (s "[pod/ttd-consul-client-zwtz2/consul] 2026-04-22T17:47:59.478577647Z ==> Starting Consul agent..."))
   (string-match kubectl-log-prefix-regex s)
   (message (match-string 1 s))
   (message (match-string 2 s))
   (message (match-string 3 s))
   (message (match-string 4 s))
   (message (match-string 5 s))))

(defun knessy--make-process-filter-logs2 (buf &optional print-pod-prefix? pod-prefix-width print-ts? ts-format)
  (let ((pod-face (knessy--next-log-face))
        (kubectl-log-prefix-regex (rx bol "[" (group (one-or-more (| letter "." "-"))) "/" (group (one-or-more (| letter digit "-"))) "/" (group (one-or-more (| letter digit "-"))) "]" (char whitespace) (group (one-or-more (| digit "-" ":" "T" "." "Z"))) (char whitespace) (group (zero-or-more anychar)))))
    (knessy--make-line-filter
     buf
     (lambda (line)
       (unless (string-match kubectl-log-prefix-regex line)
         (message "didn't match regex")
         (message line))
       (let* ((_ (match-string 1 line))
              (pod (match-string 2 line))
              (container (match-string 3 line))
              (ts (match-string 4 line))
              (msg (match-string 5 line))
              (timestamp (encode-time (parse-time-string ts)))
              ;; TODO (pgu, 20.05.2026): make this customizable
              (reformatted-ts (format-time-string (or ts-format "%Y-%m-%dT%H:%M:%S %z") timestamp)))
         (when print-pod-prefix?
           (insert
            (propertize (s-pad-right pod-prefix-width " " (format "[%s/%s] " pod container))
                        'face pod-face)))
         (when print-ts?
           (insert "[" (propertize reformatted-ts 'face 'knessy-ts-face) "] "))
         (insert msg "\n"))))))

;; TODO (pgu, 20.05.2026): move everything to knessy--shell-exec-async3
(defun knessy--shell-exec-async2 (cmd buf buferr callback)
  (knessy--shell-exec-async3
   cmd buf buferr
   (knessy--make-process-filter-to-buffer buf)
   callback))

(defun knessy--shell-exec-async3 (cmd buf buferr filter callback &optional skip-query-on-exit)
  (with-environment-variables (("KUBECONFIG" knessy-kubeconfig))
    (let* ((process (make-process
                     :name "knessy-shell-exec"
                     :command (list "/bin/bash" "-c" cmd)
                     :buffer buf
                     :stderr buferr
                     :filter filter
                     :sentinel (knessy--make-callback-sentinel callback buf buferr))))
      (process-put process 'knessy--line-buffer "")
      (when skip-query-on-exit
        (set-process-query-on-exit-flag process nil))
      process)))

(comment
 (knessy--shell-exec-async2
  "/home/pgu/bin/kubectl --context sg2-eks-p-01 api-resources --output name"
  (get-buffer-create "test")
  (get-buffer-create "testerr")
  (lambda () (message "test finished"))))


;; (defun knessy--shell-exec-async (cmd buf callback)
;;   (let* ((process (start-process-shell-command "knessy-shell-exec" nil cmd)))
;;     (set-process-filter process (knessy--make-process-filter-to-buffer buf))
;;     (set-process-sentinel process (knessy--make-callback-sentinel callback))))

(defun knessy--shell-exec (cmd buf &optional mix-stderr)
  (with-environment-variables (("KUBECONFIG" knessy-kubeconfig))
    (call-process-shell-command cmd
                                nil
                                (if mix-stderr
                                    (list buf t)
                                  buf)
                                nil)))

;; TODO: look into (with-temp-buffer) macro instead of using named buffers
;; to check this, refresh tablist _quickly_ with <g> <g> for example -- it attempts to kill a buffer with process still attached
;; which is definitely not what we want

;; async read: https://github.com/Silex/docker.el/blob/master/docker-volume.el#L88-L92 , https://github.com/skeeto/emacs-aio/issues/1
;; https://github.com/skeeto/emacs-aio/issues/19
(cl-defun knessy--shell-exec-aio (cmd buf buferr callback)
  (knessy--log 3 (format "Running %s, output: %s, stderr: %s"
                         cmd
                         (buffer-name buf)
                         (buffer-name buferr)))
  (let ((promise (aio-promise)))
    (prog1 promise
      (knessy--shell-exec-async2
       cmd buf buferr
       (lambda ()
         (aio-resolve
          promise
          callback))))))

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
