;; -*- lexical-binding: t; -*-

;; shamelessly taken from kubel

(require 's)
(require 'tramp)

(defvar knessy-kubectl)
(defvar knessy-kubeconfig)

(defcustom knessy-shell-terminal 'vterm
  "Terminal package used to shell into pods.

`vterm' uses the vterm package, `ghostel' uses the ghostel package."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "ghostel" ghostel))
  :group 'knessy)

(defun knessy--tramp-method-name (ctx ns container)
  "Return a TRAMP method name unique to CTX, NS and CONTAINER.

TRAMP caches connections by method and host.
Method names may only be alphanumeric, hence no separator.
The method bakes in context, namespace and container, so it has to differ
whenever any of them does, or a second shell would reuse the first connection."
  (format "kubectl%s"
          (substring (md5 (s-join "|" (list ctx ns (or container "")))) 0 8)))

(defun knessy--setup-tramp (ctx ns &optional container)
  "Setup a kubectl TRAMP method for CTX, NS and optionally CONTAINER.
Returns the method name."
  ;; TODO error message if resource is not pod
  (let ((method (knessy--tramp-method-name ctx ns container)))
    (setq tramp-methods (delete (assoc method tramp-methods) tramp-methods)) ;; cleanup previous tramp method
    (add-to-list 'tramp-methods
                 `(,method
                   (tramp-login-program      ,knessy-kubectl)
                   (tramp-login-args         (("--kubeconfig" ,knessy-kubeconfig)
                                              ("--context" ,ctx)
                                              ("--namespace" ,ns)
                                              ("exec" "-it")
                                              ("%h")
                                              ,@(when container `(("-c" ,container)))
                                              ("--" "sh")))
                   (tramp-remote-shell       "sh")
                   (tramp-remote-shell-args  ("-i" "-c"))))
    method))

(defun knessy--pod-containers (ctx ns pod-name)
  "Return the list of container names of pod POD-NAME in NS of CTX."
  (with-environment-variables (("KUBECONFIG" knessy-kubeconfig))
    (split-string
     (string-join
      (apply #'process-lines
             (append (split-string-and-unquote knessy-kubectl)
                     (list "--context" ctx "-n" ns "get" "pod" pod-name
                           "-o" "jsonpath={.spec.containers[*].name}")))
      " ")
     nil t)))

(defun knessy--exec-buffer-name (terminal ctx ns pod-name container)
  "Return a fresh buffer name for a TERMINAL shell into POD-NAME."
  (generate-new-buffer-name
   (s-join "_" (append (list (format "*%s" terminal) ctx ns pod-name)
                       (when container (list container))
                       (list "shell")))))

(defun knessy--exec-vterm (dir buffer-name)
  "Start vterm in DIR, in a buffer named BUFFER-NAME."
  (require 'vterm)
  (let ((default-directory dir)
        (vterm-buffer-name buffer-name)
        (vterm-shell "/bin/sh"))
    (vterm nil)))

(defun knessy--exec-ghostel (dir buffer-name)
  "Start ghostel in DIR, in a fresh buffer named BUFFER-NAME."
  (require 'ghostel)
  (let ((default-directory dir)
        (ghostel-buffer-name buffer-name)
        (ghostel-shell "/bin/sh"))
    ;; Non-nil argument always creates a new buffer instead of switching to an existing one.
    (ghostel t)))

(defun knessy-exec-pod (ctx ns pod-name &optional container)
  "Exec into POD-NAME (optionally CONTAINER) using `knessy-shell-terminal'."
  (let* ((method (knessy--setup-tramp ctx ns container))
         (dir-prefix (knessy--dir-prefix))
         (dir (format "/%s%s:%s:/" dir-prefix method pod-name))
         (buffer-name (knessy--exec-buffer-name knessy-shell-terminal ctx ns pod-name container)))
    (pcase knessy-shell-terminal
      ('vterm (knessy--exec-vterm dir buffer-name))
      ('ghostel (knessy--exec-ghostel dir buffer-name))
      (other (user-error "Unsupported `knessy-shell-terminal': %s" other)))))

(defun knessy--dir-prefix ()
  "Return the current directory prefix for a TRAMP connection."
  (or
   (when (tramp-tramp-file-p default-directory)
     (with-parsed-tramp-file-name default-directory nil
       (format "%s%s:%s|" (or hop "") method (if user (concat user "@" host) host))))
   ""))

(provide 'knessy-exec)
