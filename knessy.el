;;; TODO - write a proper emacs package header  -*- lexical-binding: t; -*-

;; dependencies: emacs 28.1, s.el, transient, dash, asoc, ht

(require 's)
(require 'asoc)
(require 'dash)

;; should all be requires here

(load "./knessy-kubectl.el")
(load "./knessy-process.el")
(load "./knessy-representation.el")
(load "./knessy-units.el")
(load "./knessy-utils.el")
(load "./knessy-comparators.el")
(load "./knessy-views.el")
(load "./knessy-cache.el")

(require 'knessy-kubectl)
(require 'knessy-process)
(require 'knessy-representation)
(require 'knessy-units)
(require 'knessy-utils)
(require 'knessy-comparators)
(require 'knessy-views)
(require 'knessy-cache)

(load "./knessy-tests.el")
(require 'knessy-tests)

(defgroup knessy nil "Customisation group for Knessy."
  :group 'extensions)

(defcustom knessy-kubeconfig (getenv "KUBECONFIG")
  "Kubectl config(s) path. May contain multiple files delimited by colon (`:`)"
  :type 'string
  :group 'knessy)

(defcustom knessy-kubectl "/usr/bin/kubectl"
  "Kubectl path. Override to use a nigthly build or a wrapper script."
  :type 'string
  :group 'knessy)

(defcustom knessy-base-buffer-name "knessy"
  "Default buffer name for Knessy."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-namespaces-string "*ALL*"
  "String depicting all namespaces."
  :type 'string
  :group 'knessy)

;; TODO: unused! use it!
(defcustom knessy-restarts-sorting 'time
  "Sorting function for RESTARTS column. Can be sorted by either
time of the last restart, or the amount of restarts."
  :type '(choice (const :tag "By time" time)
                 (const :tag "By restarts" restarts))
  :group 'knessy)

(defun knessy--reset-in-progress ()
  (interactive)
  (setq knessy--refresh-in-progress nil))

(defvar knessy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'knessy-config)
    (define-key map (kbd "d") 'knessy-do)
    (define-key map (kbd "m") 'knessy-mark)
    (define-key map (kbd "u") 'knessy-unmark)
    (define-key map (kbd "M") 'knessy-mark-all)
    (define-key map (kbd "U") 'knessy-unmark-all)
    (define-key map (kbd "r") 'knessy-rename)
    (define-key map (kbd "b") 'knessy-switch-buffer)
    (define-key map (kbd "K") 'knessy-cleanup-buffers)
    (define-key map (kbd "g") 'knessy--display-aio)
    (define-key map (kbd "G") 'knessy--reset-in-progress)
    (define-key map (kbd "j") 'knessy-jump)
    (define-key map (kbd "f") 'knessy-filter)
    map)
  "Keymap for `knessy-mode'.")

;; TODO: update on every namespace switch
(defvar-local knessy--namespace-current-all? nil)

;; Source - https://stackoverflow.com/a/22460428
;; Posted by Brian Burns, modified by community.
;; Retrieved 2026-03-23, License - CC BY-SA 4.0
(defun knessy--buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
   (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun knessy-switch-buffer (&optional all)
  "Switch to another Knessy buffer. Without ALL, lists only buffers
in Knessy mode, else lists all existing buffers."
  (interactive "P")
  ;; TODO: can we integrate with persp-mode or perspective or projects here seamlessly?..
  (let* ((candidate-names (->> (buffer-list)
                               (mapcar #'buffer-name)
                               (-filter (lambda (s)
                                          (and
                                           (s-starts-with? "*knessy" s)
                                           (if all t
                                             (eq 'knessy-mode (knessy--buffer-mode s))))))))
         (chosen-buf-name (completing-read "Choose the buffer: " candidate-names)))
    (switch-to-buffer chosen-buf-name)))

(defun knessy-cleanup-buffers ()
  "Kill all Knessy buffers"
  (interactive)
  ;; TODO: can we integrate with persp-mode or perspective or projects here seamlessly?..
  (when (yes-or-no-p "Really kill all Knessy buffers?")
    (->> (buffer-list)
         (mapcar #'buffer-name)
         (-filter (lambda (s) (s-starts-with? "*knessy" s)))
         (mapcar #'kill-buffer))))

(defun knessy-rename (name)
  "Rename current Knessy buffer to NAME."
  (interactive "MRename Knessy buffer: ")
  (let ((new-name (knessy--generate-buffer-name knessy-base-buffer-name name)))
    (rename-buffer new-name)))

;; TODO: maybe optimise mark repainting by changing the entry directly
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

(defun knessy-unmark ()
  (interactive)
  (ht-set
   knessy--marked
   (tabulated-list-get-id)
   nil)
  (knessy--repaint)
  (forward-line))

(defun knessy-unmark-all ()
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (ht-remove knessy--marked id)))
  (knessy--repaint))


;; TODO: call this when changing a namespace
(defun knessy--namespace-current-all?-update ()
  (setq knessy--namespace-current-all?
        (s-equals? knessy--namespace knessy-all-namespaces-string)))

(defun knessy--namespace-all? (namespace)
  (s-equals? namespace knessy-all-namespaces-string))

;; TODO: all these should be customizable
;; TODO: these should be set from the actual available resources,
;;   or from history, not like this

(defvar-local knessy--namespace
  "default")
(defvar-local knessy--context
  "default")
(defvar-local knessy--kind
  "pods")

(defun knessy--select-config-file (filename)
  (interactive
   (list (read-file-name "kubeconfig: " "~/.kube" nil t)))
  (let ((filename (or filename "~/.kube/config")))
    (if (file-exists-p (expand-file-name filename))
        (progn
          (setq knessy-kubeconfig filename)
          (knessy--caches-populate-async))

      (error "Kubectl config file '%s' does not exist!" filename))))

(defun knessy--print-msg ()
  (interactive)
  (message "Hello world!"))

;; TODO: *all*?..
(defun knessy--select-context ()
  (interactive)
  (setq knessy--context
        (completing-read "Context: "
                         (knessy--contexts))))

;; TODO: *all*!
(defun knessy--select-namespace ()
  (interactive)
  (setq knessy--namespace
        (completing-read "Namespace: "
                         (knessy--namespaces)))
  (knessy--namespace-current-all?-update))


;; TODO: *all*!
(defun knessy--select-kind ()
  (interactive)
  (setq knessy--kind
        (completing-read "Kind: "
                         (knessy--kinds))))

(transient-define-prefix
  knessy-config () "doc string"
  ["Configure"
     ("k" "kind" knessy--select-kind)
     ("n" "namespace" knessy--select-namespace)
     ("c" "context" knessy--select-context)
     ("f" "config-file" knessy--select-config-file)])

;; TODO: write this
(defun knessy--jump-children ()
  (interactive))

(transient-define-prefix
  knessy-jump () "Jump"
  ["Jump to"
     ("c" "children" knessy--jump-children)])

(defcustom knessy-label-selector-finish-choice
  "*FINISH*"
  "Item to indicate finishing choosing label selectors."
  :type 'string
  :group 'knessy)

(defvar knessy-label-cache-ttl 120
  "TTL for labels to live")


(defvar-local knessy--label-selectors '())

;; TODO: write this
(defun knessy--filter-label ()
  (interactive))

(transient-define-prefix
  knessy-filter () "Filter"
  ["Filter"
     ("l" "label" knessy--filter-label)])

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
          (rename (-> knessy--data (asoc-get :headers) (asoc-get :rename)))
          (widths (-> knessy--data (asoc-get :headers) (asoc-get :widths)))
          (items (asoc-get knessy--data :items)))
      (knessy--make-tablist columns rename items widths)))
  (message "Repainted!"))

;; TODO: make this display use aio for giggles!
;; TODO: this function should be using the views definitions, and become generic enough for the future use.
(defun knessy--display ()
  "Make kubectl calls and display the result."
  (interactive)
  (let* ((display-buf (current-buffer))
         (buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-display*")))
         (buferr (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-display-stderr*"))))
    (knessy--shell-exec-async2
     "kubectl --context minikube -n kube-system get deployments"
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


(aio-defun knessy--perform-calls-async (calls)
  (let ((promises '())
        (resolved '()))
    (dolist (call calls)
      (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-aio-display*")))
            (buferr (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-aio-display-stderr*")))
            (headers (asoc-get call :headers nil))
            (pre-process-ht (asoc-get call :pre-process (ht)))
            (post-process-ht (asoc-get call :post-process (ht)))
            (post-process-item (asoc-get call :post-process-item nil))
            (kind (asoc-get call :resource knessy--kind)))
        ;; TODO: clean this up and teach it various call types
        ;; TODO: continue writing the function forming the cmd string probably
        ;; TODO: dispatch table shouldn't depend on the exec style (sync/async), extract it
        (let ((promise (knessy--shell-exec-aio (knessy--call->cmd call knessy--context knessy--namespace kind)
                                               buf buferr (lambda () (knessy--parse-table-kubectl-output
                                                                      buf
                                                                      headers
                                                                      pre-process-ht
                                                                      post-process-ht
                                                                      post-process-item)))))
          (push promise promises))))
    promises))

(defun knessy--perform-calls-sync (calls)
  (let ((results '()))
    (dolist (call calls)
      (let ((buf (knessy--get-empty-buffer (generate-new-buffer-name "*knessy-display*")))
            (headers (asoc-get call :headers nil))
            (pre-process-ht (asoc-get call :pre-process (ht)))
            (post-process-ht (asoc-get call :post-process (ht)))
            (post-process-item (asoc-get call :post-process-item nil))
            (kind (asoc-get call :kind knessy--kind)))
        (knessy--shell-exec (knessy--call->cmd call knessy--context knessy--namespace kind) buf)
        (push (knessy--parse-table-kubectl-output buf headers pre-process-ht post-process-ht post-process-item) results)))
    results))

(defvar-local knessy--refresh-in-progress nil)

;; TODO: finish this!
;; TODO: add appending NAMESPACE column before NAME if it's missing + not all-namespaces
(aio-defun knessy--display-aio (&optional sync)
  "Make kubectl calls and display the result.

Set SYNC to non-nil to make the call synchronous (useful for debugging)."
  (interactive "P")
  (unless knessy--refresh-in-progress
    (setq knessy--refresh-in-progress t)
    (message "Refreshing...")
    (let* ((display-buf (current-buffer)))
      (let* ((view (ht-get knessy-views knessy--kind
                           `((:calls . (((:type . ,knessy-call-default-type)))))))
             (calls (asoc-get view :calls))
             (post-process-fn (asoc-get view :post-process-item nil))
             (results (if sync
                          (knessy--perform-calls-sync calls)
                        (let ((promises (aio-await (knessy--perform-calls-async calls)))
                              (collected '()))
                          (dolist (promise promises)  ; for some reason, mapcar won't work here. Not a big deal though.
                            (push (aio-await promise) collected))
                          collected))))
        (let* ((columns (asoc-get view :columns nil))
               ;; mix-in NAMESPACE column in front of NAME if current namespace is *ALL*
               ;; if the columns is nil, leave it as is -- NAMESPACE should be present in the kubectl output anyways
               (columns (if (and knessy--namespace-current-all? columns)
                            (knessy--insert-into-list
                             columns "NAMESPACE" (-elem-index "NAME" columns))
                          columns))
               (column-rename (asoc-get view :column-rename (ht)))
               (widths (ht-merge knessy-column-widths (asoc-get view :widths (ht))))

               (items-calls (mapcar (lambda (x) (asoc-get x :items))
                                    results)))
          ;; TODO: knessy mode should be "remembering" widths if adjusted by hand
          ;; (princ "widths:")
          ;; (princ widths)
          ;; TODO: this loop isn't really needed is it?
          ;; setup columns
          (dolist (result results)
            ;; (princ "RESULT: \n")
            ;; (princ result)
            ;; (princ "\n")
            ;; if the view misses columns, most likely it's a default view (so display whatever we got with default call)
            (unless columns
              (setq columns (-> result (asoc-get :headers) (asoc-get :static)))))
          ;; FIXME: this should gather max widths from parsing first, but then merge with configured widths as lowest priority
          ;; (dolist (item (ht-items (-> result (asoc-get :headers) (asoc-get :widths))))
          ;;   (let ((column (car item))
          ;;         (width (cadr item)))
          ;;     (ht-set widths column (max (ht-get widths column 0)
          ;;                                width)))))

          ;; post-process all the items
          ;; TODO: strictly speaking, widths have to be calculated _after_ this -- new columns may appear here!
          ;; TODO: get rid of widths calculation in parsing stage?
          ;; TODO: actually maybe set widths explicitly to avoid reading every single column again?
          (let ((merged-items (apply #'knessy--merge-items items-calls)))
            (when post-process-fn
              (mapc
               (lambda (k)
                 (funcall post-process-fn (ht-get merged-items k)))
               (ht-keys merged-items)))
            ;; set the rendering basis datastructure
            (setq knessy--data
                  `((:headers . ((:static . ,columns)
                                 (:widths . ,widths)
                                 (:rename . ,column-rename)))
                    (:items . ,merged-items))))
          ;; (message "Resulting knessy data: ")
          ;; (princ knessy--data)
          ;; paint!
          (knessy--repaint display-buf)
          (setq knessy--refresh-in-progress nil))))))

(transient-define-prefix
  knessy-do () "doc string"
  ["Do"
   ("d" "display" knessy--display)])

(defun knessy--generate-buffer-name (base suffix)
  (if suffix
      (format "*%s - %s*" base suffix)
    (format "*%s*" base)))

(defun knessy-get-new-buffer ()
  (let* ((buf-name (generate-new-buffer-name
                    (knessy--generate-buffer-name knessy-base-buffer-name nil)))
         (buf (knessy--get-empty-buffer buf-name)))
    (push buf-name knessy-buffer-list)
    buf))

;; TODO: implement switching to buffer

(defvar knessy-buffer-list nil
  "The list of Knessy buffers.")

;; TODO: add knessy-rename and knessy-list-buffers and maybe knessy-prev-next
(defun knessy-new ()
  "Create new knessy buffer."
  (interactive)
  (let* ((knessy-buffer (knessy-get-new-buffer)))
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
  ;; FIXME: this actually breaks stuff, global hook -- let's override revert for Knessy buffers?
  ;; (add-hook 'tabulated-list-revert-hook #'knessy--display-aio)
  (add-to-list 'mode-line-misc-info
               '(:eval
                 (when (eq 'knessy-mode (knessy--buffer-mode))
                   (format "%s/%s/%s"
                           knessy--context
                           knessy--namespace
                           knessy--kind))))
  (run-mode-hooks 'knessy-mode-hook))

;; TODO: next thing, implement data <-> display link to hash out the data architecture

(add-hook
 'knessy-mode-hook
 (lambda ()
   (display-line-numbers-mode -1)))

(comment
 (remove-hook
  'tabulated-list-revert-hook #'knessy--display-aio))

(provide 'knessy)
;;; knessy.el ends here
