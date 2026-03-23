;; -*- lexical-binding: t; -*-

(defvar knessy--cache-contexts
  '()
  "Caches available contexts.")

(defvar knessy--cache-kinds
  (ht)
  "Caches available resource kinds.")

(defvar knessy--cache-namespaces
  (ht)
  "Caches available namespaces.")

(defvar knessy--cache
  (ht)
  "Caches everything.")

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

(defun knessy--cache-contexts-populate-sync ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-cache-contexts*"))))
    (knessy--shell-exec
     "kubectl config get-contexts --output name"
     buf)
    (setq knessy--cache-contexts
          (knessy--read-buffer-kill buf))))

(defun knessy--cache-contexts-read ()
  ;; TODO: here might be logic to refresh once in a while
  (unless knessy--cache-contexts
    (knessy--cache-contexts-populate-sync))
  knessy--cache-contexts)

;; TODO: finish this function
(defun knessy--cache-namespaces-populate ()
  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-cache-namespaces*"))))
    (knessy--shell-exec
     "kubectl config get-contexts --output name"
     buf)
    (ht-set
     knessy--cache-namespaces
     knessy--context
     (knessy--read-buffer-kill buf))))

(comment
 (knessy--cache-contexts-populate)
 knessy--cache-contexts
 (completing-read "Context: " knessy--cache-contexts))

(defun knessy-cache-clear ()
  "Resets all the Knessy caches."
  (interactive)
  (setq knessy--cache-contexts '())
  (ht-clear knessy--cache-namespaces)
  (ht-clear knessy--cache-kinds))

;; TODO: things different between the two functions below are
;; buffer names
;; kubectl commands
;; destinations
(defun knessy--cache-namespaces-populate-async ()
  (dolist (ctx knessy--cache-contexts)
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
         (ht-set knessy--cache-namespaces ctx
                 (knessy--read-buffer-kill buf)))))))

(defun knessy--cache-resource-kinds-populate-async ()
  (dolist (ctx knessy--cache-contexts)
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
         (ht-set knessy--cache-kinds ctx
                 (knessy--read-buffer-kill buf)))))))

(defun knessy--caches-populate-async ()
  ;; should check here for cache freshness really, else
  ;; we'll be issuing too many kubectl calls for each mode activation

  (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-cache-contexts*")))
        (buferr (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-cache-contexts-stderr*"))))
    (knessy--shell-exec-async2
     "kubectl config get-contexts --output name"
     buf
     buferr
     (lambda ()
       (setq knessy--cache-contexts (knessy--read-buffer-kill buf))
       (knessy--cache-namespaces-populate-async)
       (knessy--cache-resource-kinds-populate-async)))))

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
 (knessy-cache-clear)
 knessy--cache-contexts
 knessy--cache-namespaces
 knessy--cache-kinds
 (knessy--caches-populate-async)
 (setenv "KUBECONFIG" "/home/pgu/.kube/ttd")
 (knessy--cache-contexts-populate-sync)
 (ht-get knessy--cache-namespaces "wa1-aks-p-01"))

(provide 'knessy-kubectl)
