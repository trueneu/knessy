;;; TODO - write a proper emacs package header  -*- lexical-binding: t; -*-

;; dependencies: emacs 28.1, s.el, transient

(require 's)

;; should all be requires here

(load "./knessy-kubectl.el")
(load "./knessy-process.el")
(load "./knessy-representation.el")
(load "./knessy-tests.el")
(load "./knessy-units.el")
(load "./knessy-utils.el")

(defgroup knessy nil "Customisation group for Knessy."
  :group 'extensions)

(defcustom knessy-default-kubeconfig "~/.kube/config"
  "Kubectl config(s) path. May contain multiple files delimited by colon (`:`)"
  :type 'string
  :group 'knessy)

(defcustom knessy-kubectl "/usr/bin/kubectl"
  "Kubectl path. Override to use a nigthly build or a wrapper script."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-namespaces-string "*ALL*"
  "String depicting all namespaces, instead of a single one."
  :type 'string
  :group 'knessy)

(defvar knessy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'knessy-config)
    (define-key map (kbd "d") 'knessy-do)
    map)
  "Keymap for `knessy-mode'.")

(defun knessy--expand-colons (s)
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(defvar-local knessy--kubeconfig (knessy--expand-colons knessy-default-kubeconfig))

(defvar-local knessy--namespace-all? nil)

;; TODO: call this when changing a namespace
(defun knessy--namespace-all?-update ()
  (setq knessy--namespace-all?
        (s-equals? knessy--namespace knessy-all-namespaces-string)))

;; TODO: all these should be customizable
;; TODO: these should be set from the actual available resources,
;;   or from history, not like this

(defvar-local knessy--namespace
  "default")
(defvar-local knessy--context
  "default")
(defvar-local knessy--kind
  "pods")

(defun knessy--print-msg ()
  (interactive)
  (message "Hello world!"))

(defun knessy--select-context ()
  (interactive)
  (setq knessy--context
        (completing-read "Context: "
                         (knessy--cache-contexts-read))))

(transient-define-prefix
  knessy-config () "doc string"
  ["Configure"
     ("r" "resource" knessy--print-msg)
     ("n" "namespace" knessy--print-msg)
     ("c" "context" knessy--select-context)
     ("f" "config-file" knessy--print-msg)])

(defun knessy--make-display-callback (buf)
  (lambda ()
    (let* ((parsed (knessy--parse-table-kubectl-output buf))
           (columns (-> parsed (asoc-get :headers) (asoc-get :static)))
           (items (asoc-get parsed :items)))
      (knessy--make-tablist columns items))))

(defun knessy--display ()
  "Make kubectl calls and display the result."
  (interactive)
  (let* ((buf (knessy--get-empty-buffer "*knessy-display*"))
         (buferr (knessy--get-empty-buffer "*knessy-display-stderr*")))
    (knessy--shell-exec-async2
     "kubectl --context vaf-ttd-kpop-01 -n adhoc-testing get pods"
     buf
     buferr
     (knessy--make-display-callback buf))))

(transient-define-prefix
  knessy-do () "doc string"
  ["Do"
     ("d" "display" knessy--display)])

(defun knessy ()
  (interactive)
  (knessy-mode))

(define-derived-mode knessy-mode tabulated-list-mode "Knessy"
  "Mode for Knessy buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq mode-name "Knessy")
  (setq major-mode 'knessy-mode)
  (use-local-map knessy-mode-map)
  (knessy--caches-populate-async)
  (run-mode-hooks 'knessy-mode-hook))

(add-hook
 'knessy-mode-hook
 (lambda ()
   (display-line-numbers-mode -1)))

(provide 'knessy)
;;; knessy.el ends here
