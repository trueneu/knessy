;;; TODO - write a proper emacs package header  -*- lexical-binding: t; -*-

;; dependencies: emacs 28.1, s.el, transient

(require 's)
(require 'asoc)
(require 'dash)

;; should all be requires here

(load "./knessy-kubectl.el")
(load "./knessy-process.el")
(load "./knessy-representation.el")
(load "./knessy-units.el")
(load "./knessy-utils.el")

(require 'knessy-kubectl)
(require 'knessy-process)
(require 'knessy-representation)
(require 'knessy-units)
(require 'knessy-utils)

(load "./knessy-tests.el")
(require 'knessy-tests)

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

(defcustom knessy-buffer-name "*knessy*"
  "Default buffer name for Knessy."
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
    (define-key map (kbd "m") 'knessy-mark)
    (define-key map (kbd "u") 'knessy-unmark)
    (define-key map (kbd "M") 'knessy-mark-all)
    (define-key map (kbd "U") 'knessy-unmark-all)
    map)
  "Keymap for `knessy-mode'.")

(defvar-local knessy--kubeconfig (knessy--expand-colons knessy-default-kubeconfig))

(defvar-local knessy--namespace-all? nil)

(defun knessy-mark ()
  (interactive)
  (ht-set
   knessy--marked
   (tabulated-list-get-id)
   t)
  (knessy--repaint)
  (forward-line))

(defun knessy-mark-all ()
  (interactive)
  ;; by doing this based on tabulated-list-entries, we operate just
  ;; on the entries that are actually visible
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (ht-set knessy--marked id t)))
  (knessy--repaint))

(defun knessy-unmark-all ()
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (ht-remove knessy--marked id)))
  (knessy--repaint))


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

(defvar-local knessy--data nil
  "Data that was received with the latest query.")

(defvar knessy--marked (ht)
  "Set of resource IDs that are marked (selected).")

;; TODO: this will go as it's not modular enough
(defun knessy--make-display-callback (data-buf display-buf)
  (lambda ()
    (setq knessy--data (knessy--parse-table-kubectl-output data-buf))
    (knessy--repaint display-buf)))


(defun knessy--repaint (&optional buf)
  "Repaint (or rather re-render the table).
BUF is buffer with the table, must be in Knessy mode.
If omitted, use the current one (for synchronous calls)."
  (with-current-buffer (if buf buf (current-buffer))
    (let ((columns (-> knessy--data (asoc-get :headers) (asoc-get :static)))
          (widths (-> knessy--data (asoc-get :headers) (asoc-get :widths)))
          (items (asoc-get knessy--data :items)))
      (knessy--make-tablist columns items widths))))

;; TODO: make this display use aio for giggles!
;; TODO: this function should be using the views definitions, and become generic enough for the future use.
(defun knessy--display ()
  "Make kubectl calls and display the result."
  (interactive)
  (let* ((display-buf (current-buffer))
         (buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-display*")))
         (buferr (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-display-stderr*"))))
    (knessy--shell-exec-async2
     "sleep 1 ; kubectl --context minikube -n kube-system get pods"
     buf
     buferr
     (knessy--make-display-callback buf display-buf))))

;; generic display function arch:
;; await on promises from N kubectl queries
;; after that, consolidate the data:
;; - match by IDs
;; - merge the columns
;; - run max on column widths
;; - if ID is present in one map but not in another, drop the item altogether (race with some cluster events)
;; - put the whole thing in a buffer-local variable for later manipulation

;; TODO: build a simple two-step view, like ordinary get + custom column, use it to test the above
;; TODO: make the display function use aio, to wait on multiple things in parallel

(transient-define-prefix
  knessy-do () "doc string"
  ["Do"
     ("d" "display" knessy--display)])

(defun knessy-get-buffer ()
  (generate-new-buffer knessy-buffer-name))

(defvar knessy-buffer-list nil
  "The list of Knessy buffers.")

;; TODO: add knessy-rename and knessy-list-buffers and maybe knessy-prev-next
(defun knessy-new ()
  "Create new knessy buffer."
  (interactive)
  (let* ((knessy-buffer (knessy-get-buffer)))
    (setq knessy-buffer-list (nconc knessy-buffer-list (list knessy-buffer)))
    (set-buffer knessy-buffer)
    (knessy-mode)
    (switch-to-buffer knessy-buffer)))

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
  ;; TODO: this adds a hook for every tablist mode revert?..
  ;; TODO: in these functions that repaint stuff
  (add-hook 'tabulated-list-revert-hook #'knessy--display)
  (run-mode-hooks 'knessy-mode-hook))

;; TODO: next thing, implement data <-> display link to hash out the data architecture

(add-hook
 'knessy-mode-hook
 (lambda ()
   (display-line-numbers-mode -1)))

(comment
 (remove-hook
  'tabulated-list-mode-hook #'knessy--display))

(provide 'knessy)
;;; knessy.el ends here
