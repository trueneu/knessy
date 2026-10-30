;; -*- lexical-binding: t; -*-

;; shamelessly taken from kubel

(require 's)
(require 'tramp)

(defvar knessy-kubectl)
(defvar knessy-kubeconfig)

;; The terminal packages are optional and loaded on demand.
;; Declaring their variables special makes the `let' bindings below dynamic
;; even when this file is byte-compiled without them being loaded.
(defvar vterm-buffer-name)
(defvar vterm-shell)
(defvar ghostel-buffer-name)
(defvar ghostel-shell)
(declare-function vterm "vterm")
(declare-function ghostel "ghostel")

(defcustom knessy-shell-terminal 'vterm
  "Terminal package used to shell into pods.

`vterm' uses the vterm package, `ghostel' uses the ghostel package."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "ghostel" ghostel))
  :group 'knessy)

(defun knessy--tramp-method-name (ctx ns container)
  "Return a TRAMP method name unique to CTX, NS and CONTAINER.

CTX and NS are strings with the kubectl context and namespace.
CONTAINER is a container name string, or nil for the pod's default container.

TRAMP caches connections by method and host.
The method bakes in context, namespace and container, so it has to differ
whenever any of them does, or a second shell would reuse the first connection.
Method names may only be alphanumeric, so the name is `kubectl' followed by
a short hash of the three arguments."
  (format "kubectl%s"
          (substring (md5 (s-join "|" (list ctx ns (or container "")))) 0 8)))

(defun knessy--setup-tramp (ctx ns &optional container)
  "Register a kubectl TRAMP method that execs into a pod, and return its name.

CTX and NS are strings with the kubectl context and namespace.
CONTAINER is a container name string, or nil for the pod's default container.
The method runs `kubectl exec -it <pod> [-c CONTAINER] -- sh', where the pod
is the host part of the TRAMP file name.
The name is made by `knessy--tramp-method-name'."
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
  "Return the list of container names of pod POD-NAME, as strings.

CTX and NS are strings with the kubectl context and namespace of the pod.
Only regular containers are listed, not init or ephemeral ones.
Runs kubectl synchronously, with `knessy-kubeconfig' as KUBECONFIG."
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
  "Return an unused buffer name for a shell into a pod.

TERMINAL is the terminal package symbol, `vterm' or `ghostel'.
CTX and NS are the kubectl context and namespace, POD-NAME is the pod name.
CONTAINER is a container name string, or nil when the default container is
used; it is included in the name when set."
  (generate-new-buffer-name
   (s-join "_" (append (list (format "*%s" terminal) ctx ns pod-name)
                       (when container (list container))
                       (list "shell")))))

(defun knessy--exec-vterm (dir buffer-name)
  "Start a vterm running /bin/sh, and return its buffer.

DIR is the directory to start in, here a TRAMP file name of the pod.
BUFFER-NAME is the name of the new vterm buffer."
  (require 'vterm)
  (let ((default-directory dir)
        (vterm-buffer-name buffer-name)
        (vterm-shell "/bin/sh"))
    (vterm nil)))

(defun knessy--exec-ghostel (dir buffer-name)
  "Start a ghostel terminal running /bin/sh, and return its buffer.

DIR is the directory to start in, here a TRAMP file name of the pod.
BUFFER-NAME is the name of the new ghostel buffer.
A new buffer is always created, an existing ghostel buffer is never reused."
  (require 'ghostel)
  (let ((default-directory dir)
        (ghostel-buffer-name buffer-name)
        (ghostel-shell "/bin/sh"))
    ;; Non-nil argument always creates a new buffer instead of switching to an existing one.
    (ghostel t)))

(defun knessy-exec-pod (ctx ns pod-name &optional container)
  "Open a shell inside a pod, using the terminal in `knessy-shell-terminal'.

CTX and NS are strings with the kubectl context and namespace.
POD-NAME is the name of the pod.
CONTAINER is a container name string, or nil for the pod's default container.
Returns the terminal buffer."
  (let* ((method (knessy--setup-tramp ctx ns container))
         (dir-prefix (knessy--dir-prefix))
         (dir (format "/%s%s:%s:/" dir-prefix method pod-name))
         (buffer-name (knessy--exec-buffer-name knessy-shell-terminal ctx ns pod-name container)))
    (pcase knessy-shell-terminal
      ('vterm (knessy--exec-vterm dir buffer-name))
      ('ghostel (knessy--exec-ghostel dir buffer-name))
      (other (user-error "Unsupported `knessy-shell-terminal': %s" other)))))

(defun knessy--dir-prefix ()
  "Return the TRAMP hop prefix of `default-directory', or an empty string.

When `default-directory' is already remote, the prefix lets the pod be
reached through that connection, for example `ssh:host|'.
It is empty for a local directory."
  (or
   (when (tramp-tramp-file-p default-directory)
     (with-parsed-tramp-file-name default-directory nil
       (format "%s%s:%s|" (or hop "") method (if user (concat user "@" host) host))))
   ""))

(provide 'knessy-exec)
