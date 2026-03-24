;; -*- lexical-binding: t; -*-

(defvar knessy--cache
  (ht)
  "Caches everything.")

(defun knessy--kubectl-cmd-get (ctx namespace kind &optional fmt)
  (let ((cmd (s-concat
              knessy-kubectl
              " --context " ctx
              (if (knessy--namespace-all? namespace)
                  ""
                (s-concat " -n " namespace))
              " get "  kind
              (if (knessy--namespace-all? namespace)
                  " -A"
                "")
              (if fmt
                  (s-concat " -o " fmt)
                ""))))
    (message cmd)
    cmd))


(comment
 (knessy--kubectl-cmd-get "pods" "*ALL*" "json"))


;; TODO: can generalize this to readlines and save to a list
(defun knessy--read-buffer-kill (buf)
  (let (new-cache)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((context (s-trim (thing-at-point 'line))))
          (push context new-cache))
        (forward-line 1))
      (kill-buffer))
    new-cache))

(defun knessy--query-contexts-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-contexts*"))))
    (knessy--shell-exec
     "kubectl config get-contexts --output name"
     buf)
    (knessy--read-buffer-kill buf)))

(defun knessy--contexts ()
  (knessy--cache-get
   knessy--cache
   '(:ctx)
   (lambda ()
     (knessy--query-contexts-sync))))

(defun knessy--query-namespaces-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name (s-concat "*knessy-" knessy--context "-namespaces*")))))
    (knessy--shell-exec
     "kubectl get namespaces --output custom-columns='NAME:.metadata.name' --no-headers"
     buf)
    (knessy--read-buffer-kill buf)))

(defun knessy--namespaces ()
  (knessy--cache-get
   knessy--cache
   (list :namespaces knessy--context)
   (lambda ()
     (cons knessy-all-namespaces-string
       (knessy--query-namespaces-sync)))))

(defun knessy--query-kinds-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name (s-concat "*knessy-" knessy--context "-kinds*")))))
    (knessy--shell-exec
     "kubectl api-resources --output name"
     buf)
    (knessy--read-buffer-kill buf)))

(defun knessy--kinds ()
  (knessy--cache-get
   knessy--cache
   (list :kinds knessy--context)
   (lambda ()
     (knessy--query-kinds-sync))))

(defun knessy-cache-clear ()
  "Resets all the Knessy caches."
  (interactive)
  (ht-clear knessy--cache))

;; TODO: things different between the two functions below are
;; buffer names
;; kubectl commands
;; destinations
(defun knessy--cache-namespaces-populate-async ()
  (dolist (ctx (knessy--contexts))
    (let ((buf (knessy--get-empty-buffer
                (generate-new-buffer-name
                 (concat "*knessy-cache-namespaces-" ctx "*"))))
          (buferr (knessy--get-empty-buffer
                   (generate-new-buffer-name
                    (concat "*knessy-cache-namespaces-" ctx "-stderr*")))))

      (knessy--shell-exec-async2
       (concat
        "kubectl --context "
        ctx
        " get namespaces --output custom-columns=NAME:.metadata.name --no-headers")
       buf
       buferr
       (lambda ()
         (knessy--cache-set
          knessy--cache
          (list :namespaces ctx)
          (cons knessy-all-namespaces-string (knessy--read-buffer-kill buf))))))))

(defun knessy--cache-kinds-populate-async ()
  (dolist (ctx (knessy--contexts))
    (let ((buf (knessy--get-empty-buffer
                (generate-new-buffer-name
                 (concat "*knessy-cache-resources-" ctx "*"))))
          (buferr (knessy--get-empty-buffer
                   (generate-new-buffer-name
                    (concat "*knessy-cache-resources-" ctx "-stderr*")))))
      (knessy--shell-exec-async2
       (concat
        "kubectl --context "
        ctx
        " api-resources --output name")
       buf
       buferr
       (lambda ()
         (knessy--cache-set
          knessy--cache
          (list :kinds ctx)
          (knessy--read-buffer-kill buf)))))))

(defun knessy--caches-populate-async ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-cache-contexts*")))
        (buferr (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-cache-contexts-stderr*"))))
    (knessy--shell-exec-async2
     "kubectl config get-contexts --output name"
     buf
     buferr
     (lambda ()
       (knessy--cache-set
        knessy--cache
        '(:ctx)
        (knessy--read-buffer-kill buf))
       (knessy--cache-namespaces-populate-async)
       (knessy--cache-kinds-populate-async)))))

;; TODO: kubectl forming commands already becoming dirty, generalize
;; TODO: also, resolve the kubeconfig env problem

;; TODO: what to do with clusters we couldn't log in to? like euid? (maybe redirect errors to a separate buffer with make-process)

(defun knessy-caches-refresh ()
  "Rebuild all the main caches."
  (interactive)
  (knessy-cache-clear)
  (knessy--caches-populate-async))

(defun knessy--kubectl-type->verb (type)
  (cond ((eq type 'top) "top")
        (t "get")))

(comment
 (knessy--kubectl-type->verb 'top))

(defun knessy--kubectl-construct-display (spec)
  (let ((type (asoc-get spec :type))
        (verb (knessy--kubectl-type->verb type))
        (format-type (asoc-get spec :format-type))
        (format-spec (asoc-get spec :format-spec))
        ;; TODO: finish these!
        (selectors)
        (field-selectors)))

  ;; TODO: in-progress
  (s-concat
   knessy-kubectl
   " --context "  knessy--context
   " " verb
   " " knessy--kind))



(comment
 knessy--cache
 (knessy--caches-populate-async)
 (setenv "KUBECONFIG" "/home/pgu/.kube/config:/home/pgu/.kube/k8s-local")
 (ht-get knessy--cache-namespaces "wa1-aks-p-01"))

(provide 'knessy-kubectl)
