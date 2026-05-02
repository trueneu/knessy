;; -*- lexical-binding: t; -*-

(defun knessy--env ()
  (list knessy-kubeconfig knessy--context knessy--namespace knessy--resource-type knessy--label-selectors knessy--field-selectors))

(defun knessy--env-entry->str (env)
  (let* ((config (nth 0 env))
         (ctx (nth 1 env))
         (ns (nth 2 env))
         (rt (nth 3 env))
         (labels (nth 4 env))
         (fields (nth 5 env)))
    (s-join ":" (list config ctx ns rt
                      (knessy--utils-alist->str-= labels)
                      (knessy--utils-alist->str-= fields)))))


(defun knessy--set-env (env)
  ;; if env is nil, do nothing
  (when env
    ;; TODO: maybe write first, second, etc?
    (let* ((config (nth 0 env))
           (ctx (nth 1 env))
           (ns (nth 2 env))
           (rt (nth 3 env))
           (labels (nth 4 env))
           (fields (nth 5 env)))
      (setq
       knessy-kubeconfig config
       knessy--context ctx
       knessy--namespace ns
       knessy--resource-type rt
       knessy--label-selectors labels
       knessy--field-selectors fields))))

(defun knessy--env-equal-current (env)
  ;; might become more complicated if we don't want to include regex or other filters in equality, but want to restore them
  (equal (knessy--env) env))

(defvar knessy--env-ring-length 32)
(defvar-local knessy--env-ring (make-vector knessy--env-ring-length nil))
(defvar-local knessy--env-ring-begin 0)
(defvar-local knessy--env-ring-read 0)
(defvar-local knessy--env-ring-write 1)
(defvar-local knessy--env-ring-end 1)

(defun knessy--env-ring-reset ()
  (interactive)
  (setq knessy--env-ring (make-vector knessy--env-ring-length nil)
        knessy--env-ring-begin 0
        knessy--env-ring-read 0
        knessy--env-ring-write 1
        knessy--env-ring-end 1)
  (aset knessy--env-ring 0 (knessy--env)))

(defun knessy--env-ring-show ()
  (interactive)
  (let* ((buf (get-buffer-create "*knessy-env-ring*"))
         (items (mapcar
                 #'knessy--env-ring-tablist-item
                 (number-sequence 0 (1- knessy--env-ring-length))))
         (entries (mapcar
                   (lambda (vec) (list nil vec))
                   items)))
    (with-current-buffer buf
      (tabulated-list-mode)
      (setq tabulated-list-format '[("begin" 5 t)
                                    ("end" 5 t)
                                    ("read" 5 t)
                                    ("write" 5 t)
                                    ("item" 50 t)])
      (setq tabulated-list-entries entries)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buf)))

(defun knessy--env-ring-show-pretty ()
  (interactive)
  (let* ((buf (get-buffer-create "*knessy-env-ring*"))
         (items (mapcar
                 #'knessy--env-ring-tablist-item-pretty
                 (number-sequence 0 (1- knessy--env-ring-length))))
         (entries (mapcar
                   (lambda (vec) (list nil vec))
                   items)))
    (with-current-buffer buf
      (tabulated-list-mode)
      (setq tabulated-list-format '[("POSITION" 10 t)
                                    ("ITEM" 50 t)])
      (setq tabulated-list-entries entries)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buf)))


(defun knessy--env-ring-tablist-item (idx)
  (vector
   (if (= idx knessy--env-ring-begin) "*" "")
   (if (= idx knessy--env-ring-end) "*" "")
   (if (= idx knessy--env-ring-read) "*" "")
   (if (= idx knessy--env-ring-write) "*" "")
   (knessy--env-entry->str (aref knessy--env-ring idx))))

(defun knessy--env-ring-tablist-item-pretty (idx)
  (interactive)
  (vector
   (s-concat
    (if (= idx knessy--env-ring-begin) ">>> " "")
    (if (= idx knessy--env-ring-end) "<<< " "")
    (if (= idx knessy--env-ring-read) "*" ""))
   (knessy--env-entry->str (aref knessy--env-ring idx))))

(defun knessy--env-ring-init ()
  (aset knessy--env-ring 0 (knessy--env)))

(defun knessy--env-ring-dec (v)
  (mod (+ (1- v) knessy--env-ring-length)
       knessy--env-ring-length))

(defun knessy--env-ring-inc (v)
  (mod (+ (1+ v) knessy--env-ring-length)
       knessy--env-ring-length))

(defun knessy--env-ring-read-back ()
  (unless (= knessy--env-ring-read knessy--env-ring-begin)
    (let* ((new-read (knessy--env-ring-dec knessy--env-ring-read))
           (new-write (knessy--env-ring-inc new-read)))
      (setq knessy--env-ring-read new-read
            knessy--env-ring-write new-write)))
  (aref knessy--env-ring knessy--env-ring-read))

(defun knessy--env-ring-read-forward ()
  (let* ((new-read (knessy--env-ring-inc knessy--env-ring-read))
         (new-write (knessy--env-ring-inc new-read)))
    (unless (= new-read knessy--env-ring-end)
      (setq knessy--env-ring-read new-read
            knessy--env-ring-write new-write)))
  (aref knessy--env-ring knessy--env-ring-read))

(defun knessy--env-ring-peek ()
  (aref knessy--env-ring knessy--env-ring-read))

(defun knessy--env-ring-push (v)
  (unless (equal (knessy--env-ring-peek) v)
    (let* ((new-read knessy--env-ring-write)
           (new-write (knessy--env-ring-inc knessy--env-ring-write))
           (new-end new-write))
      (aset knessy--env-ring knessy--env-ring-write v)
      (when (and (= knessy--env-ring-write knessy--env-ring-begin))
        (setq knessy--env-ring-begin (knessy--env-ring-inc knessy--env-ring-begin)))
      (setq knessy--env-ring-read new-read
            knessy--env-ring-write new-write
            knessy--env-ring-end new-end))))

(comment
 (knessy--env-ring-reset)
 (knessy--env-ring-show)
 (knessy--env-ring-read-back)
 (knessy--env-ring-read-forward)
 (knessy--env-ring-push (list "1"))
 (knessy--env-ring-push (list "2"))
 (knessy--env-ring-push (list "3"))
 (knessy--env-ring-push (list "4"))
 (knessy--env-ring-push (list "5"))
 (knessy--env-ring-push (list "6")))

(comment
 (knessy--env-append)
 (knessy--env-pop)
 (knessy--set-env (knessy--env-pop))
 knessy--env-history)

;; TODO: add config here?
(defun knessy--set-env-last-selected ()
  (when knessy--last-selected-context
    (setq knessy--context knessy--last-selected-context))
  (when knessy--last-selected-namespace
    (setq knessy--namespace knessy--last-selected-namespace))
  (when knessy--last-selected-resource-type
    (setq knessy--resource-type knessy--last-selected-resource-type)))

(provide 'knessy-env)
