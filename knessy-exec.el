;; -*- lexical-binding: t; -*-

;; TODO (pgu, 29.05.2026): need to add the container selection code as well?

;; shamelessly taken from kubel

(defun knessy--setup-tramp (ctx ns)
  "Setup a kubectl TRAMP."
  (setq tramp-methods (delete (assoc "kubectl" tramp-methods) tramp-methods)) ;; cleanup previous tramp method
  ;; TODO error message if resource is not pod
  (add-to-list 'tramp-methods
               `("kubectl"
                 (tramp-login-program      ,knessy-kubectl)
                 (tramp-login-args         (("--kubeconfig" ,knessy-kubeconfig) ("--context" ,ctx) ("--namespace" ,ns) ("exec" "-it") ("%h") ("--" "sh")))
                 (tramp-remote-shell       "sh")
                 (tramp-remote-shell-args  ("-i" "-c")))))

(defun knessy--dir-prefix ()
  "Return the current directory prefix for a TRAMP connection."
  (or
   (when (tramp-tramp-file-p default-directory)
     (with-parsed-tramp-file-name default-directory nil
       (format "%s%s:%s|" (or hop "") method (if user (concat user "@" host) host))))
   ""))

(defun knessy-exec-vterm-pod (ctx ns pod-name)
  "Exec into the pod under the cursor -> vterm."
  (interactive)
  (knessy--setup-tramp ctx ns)
  (let* ((dir-prefix (knessy--dir-prefix))
         (default-directory (format "/%skubectl:%s:/" dir-prefix pod-name))
         (vterm-buffer-name
          (generate-new-buffer-name
            (s-join "_" (list "*vterm" ctx ns pod-name "shell"))))
         (vterm-shell "/bin/sh"))
    (vterm nil)))


(provide 'knessy-exec)
