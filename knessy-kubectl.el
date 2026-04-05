;; -*- lexical-binding: t; -*-

(defvar knessy--cache
  (ht)
  "Caches everything.")

(defun knessy--kubectl-cmd-verb-kind (ctx namespace verb kind &optional fmt)
  (let ((cmd (s-concat
              knessy-kubectl
              " --context " ctx
              (if (knessy--namespace-all? namespace)
                  ""
                (s-concat " -n " namespace))
              " " verb " "  kind
              (if (knessy--namespace-all? namespace)
                  " -A"
                "")
              (if fmt
                  (s-concat " -o " fmt)
                ""))))
    (message cmd)
    cmd))

(defun knessy--call->cmd (call ctx namespace kind)
  (let ((type (asoc-get call :type)))
    (cond ((eq :get-wide type)
           (knessy--kubectl-cmd-verb-kind ctx namespace "get" kind "wide"))
          ((eq :get type)
           (knessy--kubectl-cmd-verb-kind ctx namespace "get" kind))
          ((eq :custom-columns type)
           (knessy--kubectl-cmd-verb-kind ctx namespace "get" kind (s-concat "custom-columns=" (asoc-get call :spec))))
          ((eq :jsonpath type)
           (knessy--kubectl-cmd-verb-kind ctx namespace "get" kind (s-concat "jsonpath=" (asoc-get call :spec))))
          ((eq :top type)
           (knessy--kubectl-cmd-verb-kind ctx namespace "top" kind)))))

(comment
 (knessy--kubectl-cmd-verb-kind "pods" "*ALL*" "json"))


;; TODO: can generalize this to readlines and save to a list
(defun knessy--read-buffer-kill (buf &optional no-kill)
  (let (new-cache)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((context (s-trim (thing-at-point 'line))))
          (push context new-cache))
        (forward-line 1))
      (unless no-kill
        (kill-buffer)))
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

(defun knessy--query-labels-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name (s-concat "*knessy-" knessy--context "-" knessy--kind "-labels*")))))
    (let ((cmd (s-concat
                "kubectl get "
                knessy--kind
      ;; FIXME: this should differentiate between global/namespaced, and -A
                (if (knessy--namespace-all? knessy--namespace)
                    ""
                  (s-concat " -n " knessy--namespace))
                " -o jsonpath='{range .items[*]}{.metadata.labels}{\"\\n\"}{end}' --no-headers"
                (if (knessy--namespace-all? knessy--namespace)
                    " -A"
                  ""))))
      (princ cmd)
      (knessy--shell-exec cmd buf)
      (apply
       #'ht-merge
       (mapcar
        (lambda (s) (json-parse-string s))
        (knessy--read-buffer-kill buf t))))))

(comment
 (apply #'princ '(1 2 3)))

(comment
 (let ((knessy--context "minikube")
       (knessy--namespace "kube-system")
       (knessy--kind "pods"))
   (knessy--query-labels-sync)
   (knessy--cache-get knessy--cache (list :labels knessy--context knessy--namespace knessy--kind) #'knessy--query-labels-sync))
 (-> knessy--cache
     (ht-get :labels)
     (ht-get "minikube")
     (ht-get "kube-system")
     (ht-get "pods")))


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

(defun knessy--query-kinds-namespaced-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name (s-concat "*knessy-" knessy--context "-kinds-namespaced*")))))
    (knessy--shell-exec
     "kubectl api-resources --output name --namespaced=true"
     buf)
    (knessy--make-set
     (knessy--read-buffer-kill buf))))

(defun knessy--query-kinds-global-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name (s-concat "*knessy-" knessy--context "-kinds-global*")))))
    (knessy--shell-exec
     "kubectl api-resources --output name --namespaced=false"
     buf)
    (knessy--make-set
     (knessy--read-buffer-kill buf))))

(defun knessy--kinds ()
  (knessy--cache-get
   knessy--cache
   (list :kinds knessy--context)
   (lambda ()
     (knessy--query-kinds-sync))))

(defun knessy--kinds-namespaced ()
  (knessy--cache-get
   knessy--cache
   (list :kinds-namespaced knessy--context)
   (lambda ()
     (knessy--query-kinds-namespaced-sync))))

(defun knessy--kinds-global ()
  (knessy--cache-get
   knessy--cache
   (list :kinds-global knessy--context)
   (lambda ()
     (knessy--query-kinds-global-sync))))

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
                    (concat "*knessy-cache-resources-" ctx "-stderr*"))))
          (buf-namespaced (knessy--get-empty-buffer
                           (generate-new-buffer-name
                            (concat "*knessy-cache-resources-namespaced-" ctx "*"))))
          (buferr-namespaced (knessy--get-empty-buffer
                              (generate-new-buffer-name
                               (concat "*knessy-cache-resources-namespaced-" ctx "-stderr*"))))
          (buf-global (knessy--get-empty-buffer
                       (generate-new-buffer-name
                        (concat "*knessy-cache-resources-global-" ctx "*"))))
          (buferr-global (knessy--get-empty-buffer
                          (generate-new-buffer-name
                           (concat "*knessy-cache-resources-global-" ctx "-stderr*")))))
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
          (knessy--read-buffer-kill buf))))

      (knessy--shell-exec-async2
       (concat
        "kubectl --context "
        ctx
        " api-resources --namespaced=true --output name")
       buf-namespaced
       buferr-namespaced
       (lambda ()
         (knessy--cache-set
          knessy--cache
          (list :kinds-namespaced ctx)
          (knessy--make-set
           (knessy--read-buffer-kill buf-namespaced)))))

      (knessy--shell-exec-async2
       (concat
        "kubectl --context "
        ctx
        " api-resources --namespaced=false --output name")
       buf-global
       buferr-global
       (lambda ()
         (knessy--cache-set
          knessy--cache
          (list :kinds-global ctx)
          (knessy--make-set
           (knessy--read-buffer-kill buf-global))))))))

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

(defun knessy--cache-labels-populate-async ()
  (dolist (ctx (knessy--contexts))
    ;; TODO: the buffer name should also have namespace?..
    (let ((buf (knessy--get-empty-buffer
                (generate-new-buffer-name
                 (concat "*knessy-cache-labels-" ctx "-" knessy--kind "*"))))
          (buferr (knessy--get-empty-buffer
                   (generate-new-buffer-name
                    (concat "*knessy-cache-labels-" ctx "-" knessy--kind "-stderr*")))))

      (knessy--shell-exec-async2
       (s-concat
        "kubectl get "
        knessy--kind
        ;; FIXME: this should differentiate between global/namespaced, and -A
        (if (knessy--namespace-all? knessy--namespace)
            ""
          (s-concat " -n " knessy--namespace))
        " -o jsonpath='{range .items[*]}{.metadata.labels}{\"\\n\"}{end}' --no-headers"
        (if (knessy--namespace-all? knessy--namespace)
            " -A"
          ""))
       buf
       buferr
       (lambda ()
         (knessy--cache-set
          knessy--cache
          (if (knessy--namespace-all? knessy--namespace)
              (list :labels ctx knessy--kind)
            (list :labels ctx knessy--namespace knessy--kind))
          (apply
           #'ht-merge
           (mapcar
            (lambda (s) (json-parse-string s))
            (knessy--read-buffer-kill buf t)))))))))

(comment
 (let ((knessy--context "minikube")
       (knessy--namespace "kube-system")
       (knessy--kind "pods"))
   (knessy--cache-labels-populate-async))
 (princ (ht-get (ht-get (ht-get knessy--cache :labels) "minikube") "pods")))


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
