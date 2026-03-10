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
    (define-key map (kbd "t") 'knessy-transient)
    map)
  "Keymap for `knessy-mode'.")

(defun knessy--expand-colons (s)
  (s-join ":"
    (let ((paths (s-split ":" s)))
      (mapcar #'expand-file-name paths))))

(defvar-local knessy--kubeconfig (knessy--expand-colons knessy-default-kubeconfig))

(defun knessy--print-msg ()
  (interactive)
  (message "Hello world!"))

(transient-define-prefix
  knessy-transient () "doc string"
  ["description"
     ("a" "abcd" knessy--print-msg)])

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
  (run-mode-hooks 'knessy-mode-hook))

(provide 'knessy)
;;; knessy.el ends here
