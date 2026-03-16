;; -*- lexical-binding: t; -*-

(defvar knessy--cache-contexts
  '()
  "Caches available contexts.")

(defvar knessy--cache-resource-kinds
  (ht)
  "Caches available resource kinds.")

(defvar knessy--cache-namespaces
  (ht)
  "Caches available namespaces.")

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
  ;; TODO: this cries for a helper
  ;; (kill-buffer "*knessy-cache-contexts*")
  (let ((buf (get-buffer-create "*knessy-cache-contexts*")))
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
  (let ((buf (get-buffer-create "*knessy-cache-namespaces*")))
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
  (ht-clear knessy--cache-resource-kinds))

;; TODO: things different between the two functions below are
;; buffer names
;; kubectl commands
;; destinations
(defun knessy--cache-namespaces-populate-async ()
  (dolist (ctx knessy--cache-contexts)
    ; TODO: this REALLY cries for helper
    ;; (kill-buffer (concat "*knessy-cache-namespaces-" ctx "*"))
    ;; (kill-buffer (concat "*knessy-cache-namespaces-" ctx "-stderr*"))
    (let ((buf (get-buffer-create
                (concat "*knessy-cache-namespaces-" ctx "*")))
          (buferr (get-buffer-create
                   (concat "*knessy-cache-namespaces-" ctx "-stderr*"))))

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
    ;; (kill-buffer (concat "*knessy-cache-resources-" ctx "*"))
    ;; (kill-buffer (concat "*knessy-cache-resources-" ctx "-stderr*"))
    (let ((buf (get-buffer-create
                (concat "*knessy-cache-resources-" ctx "*")))
          (buferr (get-buffer-create
                   (concat "*knessy-cache-resources-" ctx "-stderr*"))))
      (knessy--shell-exec-async2
       (concat
        "kubectl --context "
        ctx
        " api-resources --output name")
       buf
       buferr
       (lambda ()
         (ht-set knessy--cache-resource-kinds ctx
                 (knessy--read-buffer-kill buf)))))))

(defun knessy--caches-populate-async ()
  ;; (kill-buffer "*knessy-cache-contexts*")
  ;; (kill-buffer "*knessy-cache-contexts-stderr*")
  (let ((buf (get-buffer-create "*knessy-cache-contexts*"))
        (buferr (get-buffer-create "*knessy-cache-contexts-stderr*")))
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

(comment
 (knessy-cache-clear)
 knessy--cache-contexts
 knessy--cache-namespaces
 knessy--cache-resource-kinds
 (knessy--caches-populate-async)
 (setenv "KUBECONFIG" "/home/pgu/.kube/ttd")
 (knessy--cache-contexts-populate-sync)
 (ht-get knessy--cache-namespaces "wa1-aks-p-01"))

(provide 'knessy-kubectl)
