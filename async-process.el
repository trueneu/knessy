(defun filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (let (moving)
            (set-buffer (process-buffer proc))
            (setq moving (= (point) (process-mark proc)))
            (save-excursion
              ;; Insert the text, moving the process-marker.
              (goto-char (process-mark proc))
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc))))
        (set-buffer old-buffer))))

(defun sent (proc ev)
  (princ
   (format "Process %s event %s"
           proc ev))
  (princ
   (format "status %s"
           (process-status proc))))

(let* ((process (start-process-shell-command "my-process" "foo" "sleep 1 ; echo abc ; sleep 1 ; ls -l ; sleep 1 ; echo def"))
       (p-buf (process-buffer process)))
  (set-process-filter process #'filter)
  (set-process-sentinel process #'sent))

(let* ((process (start-process-shell-command "my-process" "foo" "sleep 1 ; echo abc ; sleep 1 ; exit 1"))
       (p-buf (process-buffer process)))
  (set-process-filter process #'filter)
  (set-process-sentinel process #'sent))
