;;; TODO - write a proper emacs package header  -*- lexical-binding: t; -*-

;; dependencies: emacs 28.1, s.el, transient

(require 's)

(defgroup knessy nil "Customisation group for Knessy."
  :group 'extensions)

(defcustom knessy-default-kubeconfig "~/.kube/config"
  "Kubectl config(s) path. May contain multiple files delimited by colon (`:`)"
  :type 'string
  :group 'knessy)

(defvar knessy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'knessy-config)
    map)
  "Keymap for `knessy-mode'.")

(defun knessy--expand-colons (s)
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(defvar-local knessy--kubeconfig (knessy--expand-colons knessy-default-kubeconfig))

;; TODO: all these should be customizable
;; TODO: these should be set from the actual available resources,
;;   or from history, not like this

(defvar-local knessy--namespace
  "default")
(defvar-local knessy--context
  "default")
(defvar-local knessy--resource
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

(provide 'knessy)
;;; knessy.el ends here
