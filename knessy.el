;;; TODO - write a proper emacs package header  -*- lexical-binding: t; -*-

;; TODO: reorganize all the code and fix the names this is unbearable, come up with a sane naming scheme

;; dependencies: emacs 28.1, s.el, transient, dash, asoc, ht

(require 's)
(require 'asoc)
(require 'dash)

(defvar knessy--log-level 1
  "Takes values from 0 to 5. 5 means all logging.")

(defvar knessy--debug t
  "Set to T to turn off some safety-net features.")

(defun knessy--log (level obj)
  (when (>= knessy--log-level level)
    (princ obj)
    (unless (stringp obj)
      (princ "\n"))))

(defcustom knessy-default-view-string "default"
  "String depicting default view for a resource-type."
  :type 'string
  :group 'knessy)

;; should all be requires here

(load "./knessy-kubectl.el")
(load "./knessy-process.el")
(load "./knessy-representation.el")
(load "./knessy-units.el")
(load "./knessy-utils.el")
(load "./knessy-comparators.el")
(load "./knessy-views.el")
(load "./knessy-cache.el")
(load "./knessy-env.el")

(require 'knessy-kubectl)
(require 'knessy-process)
(require 'knessy-representation)
(require 'knessy-units)
(require 'knessy-utils)
(require 'knessy-comparators)
(require 'knessy-views)
(require 'knessy-cache)
(require 'knessy-env)

(load "./knessy-tests.el")
(require 'knessy-tests)

(defgroup knessy nil "Customisation group for Knessy."
  :group 'extensions)

(defcustom knessy-kubeconfig (getenv "KUBECONFIG")
  "Kubectl config(s) path. May contain multiple files delimited by colon (`:`)"
  :type 'string
  :group 'knessy)

;; (defcustom knessy-kubectl "/usr/bin/kubectl"
;;   "Kubectl path. Override to use a nigthly build or a wrapper script."
;;   :type 'string
;;   :group 'knessy)

(defcustom knessy-kubectl "/home/pgu/bin/kubectl"
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

(defun knessy-env-go-back ()
  (interactive)
  (let ((env (knessy--env-ring-read-back)))
    (knessy--set-env env)))

(defun knessy-env-go-forward ()
  (interactive)
  (let ((env (knessy--env-ring-read-forward)))
    (knessy--set-env env)))

(transient-define-prefix knessy-log ()
    "Show logs for the selected resource(s)."
    :value '("--tail=100" "--follow")
    ["Options"
     ("-f" "Follow logs"          "--follow")
     ("-a" "All containers"       "--all-containers")
     ("-t" "Tail (lines)"         "--tail="     :reader transient-read-number-N+)
     ("-p" "Print pod/container prefix" "--prefix")
     ("-m" "Print timestamps"     "--timestamps")]
    ["Actions"
     ("l" "Display logs" knessy--logs)])

(transient-define-argument knessy-kill:--cascade ()
  :description "Cascade strategy"
  :class 'transient-option
  :key "-s"
  :argument "--cascade="
  :choices '("background" "orphan" "foreground")
  :always-read t
  :allow-empty nil)

(transient-define-prefix knessy-kill ()
    "Show logs for the selected resource(s)."
    :value '("--cascade=background" "--grace-period=-1")
    :incompatible '(("--grace-period=" "--now")
                    ("--grace-period=" "--force")
                    ("--now" "--force"))
    ["Options"
     (knessy-kill:--cascade)
     ("-f" "Force"       "--force")
     ("-g" "Grace period" "--grace-period=" :reader transient-read-number)
     ("-n" "Now" "--now")]
    ["Actions"
     ("k" "Kill resources" knessy--kill)])

(defun knessy--do-every-item-in-region (f)
  (let ((end (region-end)))
       (save-excursion
         (goto-char (region-beginning))
         (beginning-of-line)
         (while (< (point) end)
           (when-let* ((id (tabulated-list-get-id)))
             (funcall f id))
           (forward-line 1)))
       (deactivate-mark)))

(defun knessy--ids ()
  "IDs of objects to work on"
  (let* ((marked-and-visible (knessy--marked-and-visible))
         selected-ids)
    (if (use-region-p)
        (knessy--do-every-item-in-region (lambda (id)
                                           (push id selected-ids)))
      (if marked-and-visible
          (setq selected-ids marked-and-visible)
        (setq selected-ids (list (tabulated-list-get-id)))))
    selected-ids))

;; TODO (pgu, 19.05.2026): introduce additional parsing for --all-containers + timestamps
(defun knessy--logs (&optional args)
  "View logs of selected resources"
  (interactive (list (transient-args 'knessy-log)))
  (let* ((follow? (member "--follow" args))
         (all-containers? (member "--all-containers" args))
         (tail-lines (transient-arg-value "--tail=" args))
         (timestamps? (member "--timestamps" args))
         (prefix? (member "--prefix" args))

         (selected-ids (knessy--ids))

         (current-env (knessy--env))
         (log-buf (knessy--utils-fresh-kubectl-buffer "log"))
         (log-buferr (knessy--utils-fresh-kubectl-buffer "log" nil nil nil t)))

    (with-current-buffer log-buf
      (setq buffer-file-coding-system 'prefer-utf-8-unix)
      (knessy--set-env current-env)
      (dolist (id selected-ids)
          (let ((ns (knessy--id-ns id))
                (rt (knessy--id-rt id))
                (name (knessy--id-name id)))
            (knessy--kubectl-log-object-async ns rt name log-buf log-buferr follow? tail-lines all-containers? prefix? nil timestamps? t 48)))
      (knessy-log-mode)
      ;; FIXME: second time because mode change resets buffer-locals
      (knessy--set-env current-env))
    (display-buffer log-buf)))

(defvar knessy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'knessy-config)
    (define-key map (kbd "d") 'knessy-describe)
    ;; TODO (pgu, 15.05.2026): should go
    (define-key map (kbd "p") 'knessy-print-marked)
    (define-key map (kbd "k") 'knessy-kill)
    (define-key map (kbd "l") 'knessy-log)
    (define-key map (kbd "m") 'knessy-mark)
    (define-key map (kbd "u") 'knessy-unmark)
    (define-key map (kbd "M") 'knessy-mark-all)
    (define-key map (kbd "U") 'knessy-unmark-all)
    (define-key map (kbd "r") 'knessy-rename)
    (define-key map (kbd "b") 'knessy-switch-buffer)
    (define-key map (kbd "K") 'knessy-cleanup-buffers)
    (define-key map (kbd "g") 'knessy--display2)
    (define-key map (kbd "G") 'knessy--reset-in-progress)
    (define-key map (kbd "e") 'knessy-edit)
    (define-key map (kbd "j") 'knessy-jump)
    (define-key map (kbd "f") 'knessy-filter)
    (define-key map (kbd "[") 'knessy-env-go-back)
    (define-key map (kbd "]") 'knessy-env-go-forward)
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

(defun knessy--knessy-buffer? (buffer-name)
  (s-starts-with? (s-concat "*" knessy-base-buffer-name) buffer-name))

(defun knessy--knessy-main-buffer? (buffer-name)
  (s-starts-with? (s-concat "*" knessy-base-buffer-name "*") buffer-name))

(defun knessy-switch-buffer (&optional all)
  "Switch to another Knessy buffer. Without ALL, lists only buffers
in Knessy mode, else lists all existing buffers."
  (interactive "P")
  ;; TODO: can we integrate with persp-mode or perspective or projects here seamlessly?..
  (let* ((candidate-names (->> (buffer-list)
                               (mapcar #'buffer-name)
                               (-filter (lambda (s)
                                          (and
                                           (knessy--knessy-buffer? s)
                                           (if all t
                                             (eq 'knessy-mode (knessy--buffer-mode s))))))))
         (chosen-buf-name (completing-read "Choose buffer: " candidate-names)))
    (switch-to-buffer chosen-buf-name)))

(defun knessy-cleanup-buffers (&optional only-auxiliary)
  "Kill all Knessy buffers. "
  (interactive "P")
  ;; TODO: can we integrate with persp-mode or perspective or projects here seamlessly?..
  (when (yes-or-no-p (s-concat "Really kill all " (if only-auxiliary "auxiliary " "") "Knessy buffers?"))
    (if only-auxiliary
        (->> (buffer-list)
             (mapcar #'buffer-name)
             (-filter #'knessy--knessy-buffer?)
             (-remove #'knessy--knessy-main-buffer?)
             (mapcar #'kill-buffer))
      (->> (buffer-list)
           (mapcar #'buffer-name)
           (-filter #'knessy--knessy-buffer?)
           (mapcar #'kill-buffer)))))

(defun knessy-rename (name)
  "Rename current Knessy buffer to NAME."
  (interactive "MRename Knessy buffer: ")
  (let ((new-name (generate-new-buffer-name (knessy--main-buffer-name knessy-base-buffer-name name))))
    (rename-buffer new-name)))

;; TODO: maybe optimise mark repainting by changing the entry directly
(defun knessy-mark ()
  (interactive)
  (let* ((name-column-num (cl-position-if (lambda (x) (s-equals? "NAME" (first x)))
                                          tabulated-list-format)))
    (if (use-region-p)
        (knessy--do-every-item-in-region
         (lambda (id)
           (ht-set knessy--marked id t)
           (knessy--mark-tablist-entry id name-column-num)))
      (ht-set
       knessy--marked
       (tabulated-list-get-id)
       t)
      ;; (knessy--repaint)
      ;; TODO (pgu, 15.05.2026): maybe this is a bad idea.
      (knessy--mark-tablist-entry (tabulated-list-get-id) name-column-num)
      (forward-line))))


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
  (let* ((name-column-num (cl-position-if (lambda (x) (s-equals? "NAME" (first x)))
                                          tabulated-list-format)))
    (if (use-region-p)
        (knessy--do-every-item-in-region (lambda (id)
                                           (ht-set knessy--marked id nil)
                                           (knessy--unmark-tablist-entry (tabulated-list-get-id) name-column-num)))
      (ht-remove
       knessy--marked
       (tabulated-list-get-id))
      (knessy--unmark-tablist-entry (tabulated-list-get-id) name-column-num)
      (forward-line))))

(defun knessy-unmark-all ()
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (ht-remove knessy--marked id)))
  (knessy--repaint))

(defun knessy-print-marked (&optional non-visible)
  (interactive "P")
  (dolist (id (ht-keys knessy--marked))
    (when (and id
               (ht-get knessy--marked id)
               (if non-visible
                   t
                 (ht-contains? (asoc-get knessy--data :items) id)))
      (let ((ns (knessy--id-ns id))
            (rt (knessy--id-rt id))
            (name (knessy--id-name id)))
        (message "%s: %s/%s" rt ns name)))))

(defun knessy--contexts ()
  (knessy--cache-get
   knessy--cache
   '(:ctx)
   (lambda ()
     (knessy--kubectl-contexts))))

;; TODO: call this when changing a namespace
(defun knessy--namespace-current-all?-update ()
  (setq knessy--namespace-current-all?
        (s-equals? knessy--namespace knessy-all-namespaces-string)))

(defun knessy--namespace-all? (namespace)
  (s-equals? namespace knessy-all-namespaces-string))

;; TODO: all these should be customizable
;; TODO: these should be set from the actual available resources,
;;   or from history, not like this

(defcustom knessy-initial-context
  ""
  "Context at the first Knessy invocation. Leave blank to switch to current kubectl context."
  :group 'knessy
  :type 'string)

(defcustom knessy-initial-namespace
  "kube-system"
  "Namespace at the first Knessy invocation."
  :group 'knessy
  :type 'string)

(defcustom knessy-initial-resource-type
  "pods"
  "Resource type at the first Knessy invocation"
  :group 'knessy
  :type 'string)

(defvar-local knessy--namespace
  knessy-initial-namespace)

(defvar-local knessy--context
  (if (s-blank? knessy-initial-context)
      ;; FIXME: change hardcoded path here
      ;; FIXME: make it less low-level
      (let* ((buf (knessy--utils-fresh-kubectl-buffer "current-config" t t t))
             (res (if (eq 0 (knessy--shell-exec (s-concat knessy-kubectl " config current-context") buf))
                      (with-current-buffer buf
                        (s-trim (buffer-string)))
                    "default")))
        (kill-buffer buf)
        res)
    knessy-initial-context))

(comment
 (let ((buf (knessy--utils-make-buffer (generate-new-buffer-name (knessy--utils-kubectl-buffer-name "current-config" t t t)))))
      (knessy--shell-exec (s-concat knessy-kubectl " config current-context") buf)))


(defvar-local knessy--resource-type
    knessy-initial-resource-type)

(defun knessy--marked-and-visible ()
  (knessy--utils-set-intersection
   (asoc-get knessy--data :items)
   knessy--marked))


(defvar knessy--last-selected-namespace nil)
(defvar knessy--last-selected-context nil)
(defvar knessy--last-selected-resource-type nil)


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

(defun knessy--time-ms ()
  (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))

(comment
 (knessy--print-time-ms))

(defcustom knessy-reset-label-selectors-on-resource-type-change nil
  "Whether to reset label filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-field-selectors-on-resource-type-change nil
  "Whether to reset field filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-label-selectors-on-namespace-change nil
  "Whether to reset label filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-field-selectors-on-namespace-change nil
  "Whether to reset field filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-label-selectors-on-context-change nil
  "Whether to reset label filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defcustom knessy-reset-field-selectors-on-context-change nil
  "Whether to reset field filters on resource type change."
  :group 'knessy
  :type 'boolean)

(defun knessy--set-context (ctx)
  (setq knessy--context ctx)
  (setq knessy--last-selected-context knessy--context)
  (when knessy-reset-label-selectors-on-context-change
    (setq knessy--label-selectors '()))
  (when knessy-reset-field-selectors-on-context-change
    (setq knessy--field-selectors '())))

;; TODO: *all*?.. contexts
(defun knessy--select-context ()
  (interactive)
  (when-let* ((new-ctx (completing-read "Context: "
                                        (knessy--contexts))))
    (knessy--set-context new-ctx)))

(defun knessy--set-namespace (ns)
  (setq knessy--namespace
        ns)
  (knessy--namespace-current-all?-update)
  (setq knessy--last-selected-namespace knessy--namespace)
  (when knessy-reset-label-selectors-on-namespace-change
    (setq knessy--label-selectors '()))
  (when knessy-reset-field-selectors-on-namespace-change
    (setq knessy--field-selectors '())))

(defun knessy--select-namespace ()
  (interactive)
  (let ((new-ns (completing-read "Namespace: "
                                 (knessy--namespaces))))
    (knessy--set-namespace new-ns)))

(defun knessy--set-resource-type (resource-type)
  (setq knessy--resource-type resource-type)
  (setq knessy--view
        (or (ht-get knessy--last-selected-view knessy--resource-type nil)
            (asoc-get knessy-default-view-alist knessy--resource-type knessy-default-view-string)))
  (setq knessy--table-remember-pos nil)
  (setq knessy--last-selected-resource-type knessy--resource-type)
  (when knessy-reset-label-selectors-on-resource-type-change
    (setq knessy--label-selectors '()))
  (when knessy-reset-field-selectors-on-resource-type-change
    (setq knessy--field-selectors '())))

;; TODO: *all*!
;; TODO: reset labels and fields on resource type, ns, ctx change? make it configurable?
;; TODO: maybe also reset the regex filter
(defun knessy--select-resource-type ()
  (interactive)
  (when-let* ((new-rt (completing-read "Kind: "
                                       (knessy--resource-types))))
    (knessy--set-resource-type new-rt)))

(defvar knessy--resource-type-children
  (ht ('deployment 'rs)
      ('argo-rollout 'rs)
      ('statefulset 'pod)
      ('daemonset 'pod)
      ('rs 'pod)
      ;; should be in some special list
      ('node 'pod)))

;; TODO: it would be great to just pass some jsonpath like .spec.selector.matchLabels=labels or metadata.name=spec.nodeName for jumping to children and back

(defvar knessy--resource-type-str->sym
  (ht ("deployments.apps" 'deployment)
      ("Deployment" 'deployment)
      ("statefulsets.apps" 'statefulset)
      ("StatefulSet" 'statefulset)
      ("daemonsets.apps" 'daemonset)
      ("DaemonSet" 'daemonset)
      ("replicasets.apps" 'rs)
      ("ReplicaSet" 'rs)
      ("pods" 'pod)
      ("Pod" 'pod)
      ("nodes" 'node)
      ("Node" 'node)
      ("rollouts.argoproj.io" 'argo-rollout)
      ("Rollout" 'argo-rollout)))

(defvar knessy--resource-type-sym->str-list
  (let ((res (ht)))
    (dolist (item (ht-items knessy--resource-type-str->sym) res)
      (let ((str (car item))
            (sym (cadr item)))
        (unless (ht-contains? res sym)
          (ht-set res sym '()))
        (ht-update-with! res sym (lambda (l) (cons str l)) '())))))

(defun knessy--resource-type-child (owner-resource-type)
  (knessy--res)
  (ht-get knessy--resource-type-children
          (knessy--resource-type-str->sym owner-resource-type)))

(defvar-local
    knessy--view
    knessy-default-view-string)

(defvar knessy--last-selected-view (ht))

(defvar-local
    knessy--table-remember-pos
    t)

(defun knessy--set-view (resource-type view)
  (let ((old-view knessy--view))
    (setq knessy--view view)
    (ht-set knessy--last-selected-view resource-type view)
    (unless (eq old-view view)
      (setq knessy--table-remember-pos nil))))

(defun knessy--select-view ()
  (interactive)
  (when-let* ((new-view (completing-read
                         "View: "
                         (ht-get knessy--views-by-resource-type knessy--resource-type (list knessy-default-view-string))
                         nil
                         t)))
    (knessy--set-view knessy--resource-type new-view)))

(transient-define-prefix
  knessy-config () "doc string"
  ["Configure"
     ("t" "resource-type" knessy--select-resource-type)
     ("n" "namespace" knessy--select-namespace)
     ("c" "context" knessy--select-context)
     ("f" "config-file" knessy--select-config-file)
     ("v" "view" knessy--select-view)])

(defcustom knessy-temp-file-dir (file-name-concat temporary-file-directory "knessy")
  "Directory to put temporary Knessy files")

(defun knessy-edit (&optional json?)
  (interactive "P")
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (current-env (knessy--env))
         filename
         file-buf)
    (let* ((temporary-file-directory knessy-temp-file-dir))
      (setq filename (make-temp-file
                      (s-concat "knessy_"
                                knessy--context "_"
                                rt "_"
                                (if ns (s-concat ns "_") "")
                                name "_")
                      nil
                      (if json? ".json" ".yaml")))
      (setq file-buf (find-file-literally filename)))
    (with-current-buffer file-buf
      (setq buffer-file-coding-system 'prefer-utf-8-unix)
      (knessy--set-env current-env)
      (knessy--kubectl-get-object-sync ns rt name file-buf (if json? :json :yaml))
      (goto-char (point-min))
      (save-buffer)
      (if json?
          (knessy-json-mode)
        (knessy-yaml-mode))
      ;; FIXME: second time because mode change resets buffer-locals
      (knessy--set-env current-env))
    (display-buffer file-buf)))

(defun knessy-describe ()
  (interactive "")
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj-buf (knessy--kubectl-describe-object-sync ns rt name)))
    (with-current-buffer obj-buf
      (knessy-describe-mode)
      (goto-char (point-min)))
    (display-buffer obj-buf)))

;; TODO (pgu, 19.05.2026): maybe do prefix via transient so no-prompt is obvious
;; FIXME (pgu, 19.05.2026): doesn't work with *ALL* namespaces
(defun knessy--kill (&optional args prefix)
  (interactive (list (transient-args 'knessy-kill) current-prefix-arg))
  (let* ((force? (member "--force" args))
         (grace-period (transient-arg-value "--grace-period=" args))
         (now? (member "--now" args))
         (selected-ids (knessy--ids))
         (kill-buf (knessy--utils-fresh-kubectl-buffer "kill"))
         (kill-buferr (knessy--utils-fresh-kubectl-buffer "kill" nil nil nil t)))
    ;; TODO (pgu, 18.05.2026): here, detect number of different resource types and bulk-delete them
    (if (or prefix (yes-or-no-p (s-concat "Killing " (s-join ", " (mapcar #'knessy--id-name selected-ids)) ", proceed? ")))
        (dolist (id selected-ids)
          (let ((ns (knessy--id-ns id))
                (rt (knessy--id-rt id))
                (name (knessy--id-name id)))
            (knessy--kubectl-delete-object-async ns rt name kill-buf kill-buferr force? grace-period now?)))
      (user-error "Aborted"))))


;; TODO (pgu, 19.05.2026): extract to knessy-transient.el
(defun transient-read-number (prompt initial-input history)
  "Read a natural number (excluding zero) and return it as a string."
  (transient--read-number-any prompt initial-input history))

(defun transient--read-number-any (prompt initial-input history)
  (save-match-data
    (cl-block nil
      (while t
        (let ((str (read-from-minibuffer prompt initial-input nil nil history)))
          (when (or (string-equal str "")
                    (string-match-p "\\`\\(0\\|-?[1-9][0-9]*\\)\\'"
                                    str))
            (cl-return str)))
        (message "Please enter an integer number.")
        (sit-for 1)))))

(comment
 (string-match-p "\\`\\(0\\|-?[1-9][0-9]*\\)\\'"
                                    "-123"))

(comment
 (append (vector 1 2 3) nil)
 (asoc)
 (asoc-put! '((apple . 1) (banana . 2)) 'a 'b)
 (let ((my-alist '((apple . 1) (banana . 2))))
  ;; Update existing key
  (asoc-put! my-alist 'apple 10)
  ;; Add new key
  (asoc-put! my-alist 'orange 3)
  my-alist)
 (asoc-zip '("spec.nodeName") '("blah")))

(defun knessy--jump-namespace ()
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id)))
    (when (null ns)
      (user-error "This command should be used on a namespaced object."))
    (knessy--set-namespace ns)
    (setq knessy--field-selectors nil)
    (setq knessy--label-selectors nil)
    (knessy--display2)))

(defun knessy--jump-node ()
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name))
         (obj-sym (ht-get knessy--resource-type-str->sym rt)))
    (when (not (eq obj-sym 'pod))
      (user-error "This command should be used on a pod."))
    (when-let* ((nodename (ht-get* obj "spec" "nodeName")))
      (knessy--set-resource-type "nodes")
      (setq knessy--field-selectors (asoc-zip '("metadata.name") (list nodename)))
      (setq knessy--label-selectors nil)
      (knessy--display2))))

(defun knessy--jump-node-pods ()
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name))
         (obj-sym (ht-get knessy--resource-type-str->sym rt)))
    (when (not (or (eq obj-sym 'pod)
                   (eq obj-sym 'node)))
      (user-error "This command should be used on a pod or a node."))
    (when-let* ((nodename
                 (cond ((eq obj-sym 'pod)
                        (ht-get* obj "spec" "nodeName"))
                       ((eq obj-sym 'node)
                        (ht-get* obj "metadata" "name")))))
      (knessy--set-resource-type "pods")
      (knessy--set-namespace knessy-all-namespaces-string)
      (setq knessy--field-selectors (asoc-zip '("spec.nodeName") (list nodename)))
      (setq knessy--label-selectors nil)
      (knessy--display2))))

(defun knessy--jump-children ()
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name))
         (owner-sym (ht-get knessy--resource-type-str->sym rt))
         (children-resource-type (knessy--child-resource-type rt)))
    (cond ((eq owner-sym 'node)
           ;; TODO: implement this!
           (progn
             ;; (setq knessy--resource-type children-resource-type)
             (knessy--set-resource-type children-resource-type)
             (setq knessy--field-selectors (asoc-zip '("spec.nodeName") (list name)))
             (setq knessy--label-selectors nil)
             (knessy--display2)))

          (t
           (if-let* ((spec (ht-get obj "spec")))
            (if-let* ((selector (ht-get spec "selector")))
                (if-let* ((match-labels (ht-get selector "matchLabels")))
                    (progn
                      (knessy--set-resource-type children-resource-type)
                      (setq knessy--label-selectors (ht->alist match-labels))
                      (setq knessy--field-selectors nil)
                      (knessy--display2))

                  (error "Object selector has no matchLabels"))
              (error "Object spec has no selector"))
            (error "Object has no spec"))))))

(defun knessy--kind->resource-type (kind)
  (let* ((sym (ht-get knessy--resource-type-str->sym kind))
         (types (ht-get knessy--resource-type-sym->str-list sym)))
    (cl-loop for type in types
             when (ht-contains? (knessy--resource-types-set) type)
             return type)))

(defun knessy--child-resource-type (owner-resource-type)
  (let* ((sym (ht-get knessy--resource-type-str->sym owner-resource-type))
         (child-sym (ht-get knessy--resource-type-children sym))
         (types (ht-get knessy--resource-type-sym->str-list child-sym)))
    (cl-loop for type in types
             when (ht-contains? (knessy--resource-types-set) type)
             return type)))

(comment
 (ht-contains? (knessy--resource-types-set) "replicasets.apps"))

;; TODO: handle the errors (no owner, no metadata etc)
(defun knessy--jump-owner ()
  (interactive)
  (let* ((selected-id (tabulated-list-get-id))
         (ns (knessy--id-ns selected-id))
         (rt (knessy--id-rt selected-id))
         (name (knessy--id-name selected-id))
         (obj (knessy--kubectl-get-object-parsed-sync ns rt name)))
    (if-let* ((metadata (ht-get obj "metadata")))
        (if-let* ((owner-refs (ht-get metadata "ownerReferences")))
            (if (> (length owner-refs) 0)
                (let* ((owner-ref (aref owner-refs 0))
                       (owner-name (ht-get owner-ref "name"))
                       (owner-kind (ht-get owner-ref "kind"))
                       (owner-resource-type (knessy--kind->resource-type owner-kind)))
                  (message "owner kind %s" owner-kind)
                  (message "owner resource type %s" owner-resource-type)
                  (knessy--set-resource-type owner-resource-type)
                  (setq knessy--field-selectors `(("metadata.name" . ,owner-name)))
                  (setq knessy--label-selectors nil)
                  (call-interactively #'knessy--display2))
              (error "Object ownerReferences are empty"))
          (error "Object metadata has no ownerReferences"))
      (error "Object has no metadata"))))

(defvar-local knessy--field-selectors '())

(defvar knessy--field-selectors-fields-ht
  (ht ('pod '("spec.nodeName"
              "spec.restartPolicy"
              "spec.schedulerName"
              "spec.serviceAccountName"
              "spec.hostNetwork"
              "status.phase"
              "status.podIP"
              "status.podIPs"
              "status.nominatedNodeName"))
      ('event '("involvedObject.kind"
                "involvedObject.namespace"
                "involvedObject.name"
                "involvedObject.uid"
                "involvedObject.apiVersion"
                "involvedObject.resourceVersion"
                "involvedObject.fieldPath"
                "reason"
                "reportingComponent"
                "source"
                "type"))
      ('secret '("type"))
      ('namespace '("status.phase"))
      ('rs '("status.replicas")) ;; FIXME: doesn't work?
      ('rscontroller '("status.replicas"))
      ('job '("status.successful"))
      ('node '("spec.unschedulable"))
      ('csr '("spec.signerName"))))

(defvar knessy--field-selectors-fields-common
  '("metadata.name" "metadata.namespace"))

(defun knessy--field-selectors-fields (resource-type)
  (append
   knessy--field-selectors-fields-common
   (ht-get knessy--field-selectors-fields-ht
           (ht-get knessy--resource-type-str->sym resource-type))))

(defcustom knessy-field-selector-finish-choice "*FINISH*"
  "String to use to finish field selectors choosing process"
  :group 'knessy
  :type 'string)

(comment
 (read-string)
 (append '(1) '(2)))

;; TODO: finish this, so we can jump to children
(defun knessy--filter-field-selector ()
  (interactive)
  (let ((current-field nil)
        (current-value nil)
        (new-filters '())
        ;; if the namespace is ALL or the resource-type is global
        (fields (knessy--field-selectors-fields knessy--resource-type)))
    (while (not (s-equals? current-field knessy-label-selector-finish-choice))
      (setq current-field
            (completing-read
             (s-concat "(" (if new-filters
                               (knessy--utils-alist->str-= new-filters)
                             (knessy--utils-alist->str-= knessy--field-selectors))
                       ") Select field: ")
             (cons knessy-field-selector-finish-choice
                   fields)))
      (unless (s-equals? current-field knessy-field-selector-finish-choice)
        ;; TODO: I'm pretty sure we can do completing-read here
        (setq current-value
              (read-string
               "Enter value: "))
        (push (cons current-field current-value) new-filters)))
    (setq knessy--field-selectors new-filters)))

(comment
 (catch)
 (ht-get nil 'key))

(transient-define-prefix
  knessy-jump () "Jump"
  ["Jump to"
   ("c" "children" knessy--jump-children)
   ("o" "owner" knessy--jump-owner)
   ("n" "node" knessy--jump-node)
   ("p" "node pods" knessy--jump-node-pods)
   ("N" "namespace" knessy--jump-namespace)])

(defcustom knessy-label-selector-finish-choice
  "*FINISH*"
  "Item to indicate finishing choosing label selectors."
  :type 'string
  :group 'knessy)

(defvar knessy-label-cache-ttl 120
  "TTL for labels to live")

(defvar-local knessy--label-selectors '())

(defun knessy--resource-type-namespaced? (resource-type)
  "Return T if the current resource-type is namespaced, nil if it's global."
  (ht-contains? (knessy--resource-types-namespaced) resource-type))

(defun knessy--resource-type-global? (resource-type)
  (not (knessy--resource-type-namespaced? resource-type)))

(defun knessy--filter-label ()
  (interactive)
  (let ((current-label nil)
        (current-value nil)
        (new-selectors '())
        ;; if the namespace is ALL or the resource-type is global
        (lookup-keys (if (or (knessy--namespace-all? knessy--namespace) (knessy--resource-type-global? knessy--resource-type))
                         (list :labels knessy--context knessy--resource-type)
                       (list :labels knessy--context knessy--namespace knessy--resource-type))))
    (while (not (s-equals? current-label knessy-label-selector-finish-choice))
      (setq current-label
            (completing-read
             (s-concat "(" (if new-selectors
                               (knessy--utils-alist->str-= new-selectors)
                             (knessy--utils-alist->str-= knessy--label-selectors))
                       ") Select label: ")
             (cons knessy-label-selector-finish-choice
                   (ht-keys
                    (knessy--cache-get knessy--cache lookup-keys #'knessy--kubectl-labels)))))
      (unless (s-equals? current-label knessy-label-selector-finish-choice)
        (setq current-value
              (completing-read
               "Select value: "
               (ht-keys
                (ht-get
                 (knessy--cache-get knessy--cache lookup-keys #'knessy--kubectl-labels)
                 current-label))))
        (push (cons current-label current-value) new-selectors)))

    (setq knessy--label-selectors new-selectors)))

(comment
 (let ((knessy--context "minikube")
       (knessy--namespace "kube-system")
       (knessy--resource-type "pods"))
   (knessy--filter-label)))

(defvar-local knessy--regex "")
(defvar-local knessy--regex-hide nil)

(defun knessy--set-regex ()
  (interactive)
  (let ((regex (read-string "Regex: " knessy--regex)))
    ;; TODO: verify if regex is valid
    (setq knessy--regex regex))
  ;; TODO (pgu, 19.05.2026): maybe remove repaint?
  (knessy--repaint))


(defun knessy--toggle-regex-hide ()
  (interactive)
  (setq knessy--regex-hide (not knessy--regex-hide))
  (knessy--log 4 (format "Regex Hide is now %s" knessy--regex-hide))
  (message "Regex filter hide is %s" (if knessy--regex-hide "on" "off"))
  ;; TODO (pgu, 19.05.2026): maybe remove repaint?
  (knessy--repaint))


(defun knessy--clear-filters ()
  (interactive)
  (setq knessy--field-selectors '())
  (setq knessy--label-selectors '()))

(transient-define-prefix
  knessy-filter () "Filter"
  ["Filter"
   ("c" "clear" knessy--clear-filters)
   ("l" "label" knessy--filter-label)
   ("f" "field" knessy--filter-field-selector)
   ("r" "regex" knessy--set-regex)
   ("h" "regex-hide" knessy--toggle-regex-hide)])

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
      (if (and (ht? items) (> (ht-size items) 0))
          (knessy--make-tablist columns rename items widths)
        (let ((item (ht ("NAME" "No resources found."))))
          (setq knessy--table-remember-pos nil)
          (knessy--make-tablist '("NAME")
                                (ht)
                                (ht (nil item))
                                (ht ("NAME" 8)))))))
  (knessy--log 1 "Done!")
  (knessy--log 4 (format "Now: %s" (current-time-string))))

(comment
 (let ((abc 'def))
   (ht (nil abc))))

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
      (let ((buf (knessy--utils-fresh-kubectl-buffer "aio-display" t t t))
            (buferr (knessy--utils-fresh-kubectl-buffer "aio-display" t t t t))
            (headers (asoc-get call :headers nil))
            (pre-process-ht (asoc-get call :pre-process (ht)))
            (post-process-ht (asoc-get call :post-process (ht)))
            (post-process-item (asoc-get call :post-process-item nil))
            (knessy--resource-type (asoc-get call :resource knessy--resource-type)))
        ;; TODO: clean this up and teach it various call types
        ;; TODO: continue writing the function forming the cmd string probably
        ;; TODO: dispatch table shouldn't depend on the exec style (sync/async), extract it
        (let ((promise (knessy--shell-exec-aio (knessy--kubectl-call->cmd call)
                                               buf buferr (lambda ()
                                                            (prog1 (knessy--parse-table-kubectl-output
                                                                    buf
                                                                    headers
                                                                    pre-process-ht
                                                                    post-process-ht
                                                                    post-process-item)
                                                              (if knessy-shell-kill-success-buffers
                                                                  (kill-buffer buf)))))))

          (push promise promises))))
    promises))

(defun knessy--kill-success-buffer-maybe (buf)
  (if knessy-shell-kill-success-buffers
      (kill-buffer buf)))

(defun knessy--perform-calls-sync (calls)
  (let ((results '()))
    (dolist (call calls)
      (let ((buf (knessy--utils-fresh-kubectl-buffer "display" t t t))
            (headers (asoc-get call :headers nil))
            (pre-process-ht (asoc-get call :pre-process (ht)))
            (post-process-ht (asoc-get call :post-process (ht)))
            (post-process-item (asoc-get call :post-process-item nil))
            (knessy--resource-type (asoc-get call :resource-type knessy--resource-type)))
        (knessy--log 4 (format "in knessy--perform-calls-sync, current buffer: %s" (buffer-name (current-buffer))))
        (knessy--shell-exec (knessy--kubectl-call->cmd call) buf)
        (push (knessy--parse-table-kubectl-output buf headers pre-process-ht post-process-ht post-process-item) results)
        (knessy--kill-success-buffer-maybe buf)))
    results))

(defvar-local knessy--refresh-in-progress nil
  "If T, the buffer refresh is already in progress.
Made so spamming refreshes doesn't result in 100 of kubectl calls.")

(aio-defun knessy--display2-aio ()
  (interactive)
  (knessy--log 3 (format "Display AIO called at %s" (current-time-string)))
  (unless knessy--refresh-in-progress
    (if knessy--debug
        (setq knessy--refresh-in-progress nil)
      (setq knessy--refresh-in-progress t))
    (knessy--log 4 "Refreshing...")
    (let* ((display-buf (current-buffer)))
      (knessy--cache-labels-populate-async knessy--context knessy--namespace knessy--resource-type)
      (let* ((view (ht-get knessy-views (cons knessy--resource-type knessy--view)
                           `((:calls . (((:type . ,knessy-call-default-type)))))))
             (calls (asoc-get view :calls))
             (post-process-fn (asoc-get view :post-process-item nil))
             (results (let ((promises (aio-await (knessy--perform-calls-async calls)))
                            (collected '()))
                          (dolist (promise promises)  ; for some reason, mapcar won't work here. Not a big deal though.
                            (push (aio-await promise) collected))
                          collected)))
        (knessy--display2-common-part display-buf view results post-process-fn)))))

;; FIXME: I don't understand why this function is needed, but (commandp 'knessy--display2-aio) is nil?..
(defun knessy--display2 (&optional sync)
  (interactive "P")
  (knessy--env-ring-push (knessy--env))
  (if sync
        (knessy--display2-sync)
      (knessy--display2-aio)))

(comment
 (commandp 'knessy--display2-aio)
 (commandp 'knessy--display2-sync)
 (commandp 'knessy--display2-aio-wrap))

;; TODO: refactor this further, this may be cleaner
(defun knessy--display2-sync ()
  (interactive)
  (knessy--log 3 (format "Display sync called at %s" (current-time-string)))
  (unless knessy--refresh-in-progress
    (if knessy--debug
        (setq knessy--refresh-in-progress nil)
      (setq knessy--refresh-in-progress t))
    (knessy--log 4 "Refreshing...")
    (let* ((display-buf (current-buffer)))
      (knessy--cache-labels-populate-async knessy--context knessy--namespace knessy--resource-type)
      (let* ((view (ht-get knessy-views (cons knessy--resource-type knessy--view)
                           `((:calls . (((:type . ,knessy-call-default-type)))))))
             (calls (asoc-get view :calls))
             (post-process-fn (asoc-get view :post-process-item nil))
             (results (knessy--perform-calls-sync calls)))
        (knessy--display2-common-part display-buf view results post-process-fn)))))


(defun knessy--display2-common-part (display-buf view results post-process-fn)
  (knessy--log 5 "Current buffer name: ")
  (knessy--log 5 (buffer-name))
  (knessy--log 4 "Got calls results!")
  (let* ((columns (asoc-get view :columns nil))
         ;; mix-in NAMESPACE column in front of NAME if current namespace is *ALL*
         ;; if the columns is nil, leave it as is -- NAMESPACE should be present in the kubectl output anyways
         (columns (if (and knessy--namespace-current-all? columns)
                      (knessy--utils-insert-into-list
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
    (knessy--log 5 "Current buffer name: ")
    (knessy--log 5 (buffer-name))
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
    (knessy--log 4 "Merging items from multiple calls...")
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
    (knessy--log 5 "Resulting knessy data: ")
    (knessy--log 5 knessy--data)
    ;; paint!
    (knessy--repaint display-buf)
    (knessy--log 5 "Current buffer name: ")
    (knessy--log 5 (buffer-name))
     ;; dumb fix
     ;; (display-buffer display-buf)
    (setq knessy--refresh-in-progress nil)))

(defun knessy--main-buffer-name (base suffix)
  (if (s-blank? suffix)
      (format "*%s*" base)
    (format "*%s - %s*" base suffix)))


(defun knessy-get-new-buffer ()
  (let* ((buf-name (generate-new-buffer-name
                    (knessy--main-buffer-name knessy-base-buffer-name nil)))
         (buf (knessy--utils-make-buffer buf-name)))
    (push buf-name knessy-buffer-list)
    buf))

(defvar knessy-buffer-list nil
  "The list of Knessy buffers.")


;; TODO: add knessy-rename and knessy-list-buffers and maybe knessy-prev-next (how to maintain order?)
(defun knessy-new ()
  "Create new knessy buffer."
  (interactive)
  (let* ((knessy-buffer (knessy-get-new-buffer)))
    ;; TODO (pgu, 17.05.2026): huh, should there be a setup step that's run only once or something?
    (make-directory knessy-temp-file-dir t)
    (knessy--set-resource-type knessy--resource-type)
    ;; FIXME: is it wrong use of nconc there?
    (setq knessy-buffer-list (nconc knessy-buffer-list (list knessy-buffer)))
    (set-buffer knessy-buffer)
    (knessy-mode)
    (switch-to-buffer knessy-buffer)
    (knessy--set-env-last-selected)))

(defun knessy ()
  (interactive)
  (knessy-mode))

;; TODO (pgu, 17.05.2026): separate user-facing knessy--select-context,
;; knessy--select-buffer and the likes into 2 parts: 1) user-facing one, with
;; (completing-read) and all that; 2) internal one, that for instance resets the
;; view when the resource type is changed

(define-derived-mode knessy-mode tabulated-list-mode "Knessy"
  "Mode for Knessy buffers."
  (buffer-disable-undo)
  (knessy--caches-populate-async)
  ;; FIXME: this actually breaks stuff, global hook -- let's override revert for Knessy buffers?
  ;; (add-hook 'tabulated-list-revert-hook #'knessy--display-aio)

  ;; FIXME: hack to work around my kubel hacks
  (setq mode-line-misc-info '())
  (add-to-list 'mode-line-misc-info
               '(:eval
                 (when (eq 'knessy-mode (knessy--buffer-mode))
                   (concat
                    (format "%s/%s/%s"
                            knessy--context
                            knessy--namespace
                            knessy--resource-type)
                    (if (s-equals? knessy-default-view-string knessy--view)
                        ""
                      (s-concat " (" knessy--view ")"))
                    (if knessy--label-selectors
                        (s-concat " " (knessy--utils-alist->str-= knessy--label-selectors))
                      "")
                    (if knessy--field-selectors
                        (s-concat " " (knessy--utils-alist->str-= knessy--field-selectors))
                        "")
                    (if (s-blank? knessy--regex)
                        ""
                      (format " /%s/" knessy--regex)))))))

;; TODO: next thing, implement data <-> display link to hash out the data architecture

;; TODO: save to a temp file
;; run kubectl apply -f file with namespace context config etc
;; reopen again if refused with error
;; close the buffer

;; TODO: ask user to save if needed
(defun knessy-apply ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (buf-apply (knessy--utils-fresh-kubectl-buffer (s-concat "apply_" filename) t t t)))
    (when (buffer-modified-p)
      (if (y-or-n-p "Buffer was modified. Save and continue? ")
          (save-buffer)
        (user-error "Aborted")))
    (when (knessy--validate)
      (knessy--kubectl-apply-file-sync buf-apply filename)
      ;; TODO (pgu, 17.05.2026): add toggles for deleting/killing buffer behaviour
      (kill-buffer)
      (delete-file filename)
      (message "Applied!"))))

(defun knessy--kubectl-file-sync-arbitrary (verb kubectl-fn prompt-fmt success-msg)
  "Run KUBECTL-FN on the file backing the current buffer.
VERB names the action in the kubectl buffer name (e.g. \"apply\").
PROMPT-FMT is the confirmation prompt format with two %s slots for
context and namespace."
  (let* ((knessy--context knessy--last-selected-context)
         (knessy--namespace knessy--last-selected-namespace)
         ;; FIXME (pgu, 18.05.2026): this one is just for the buffer naming really
         (knessy--resource-type knessy--last-selected-resource-type)
         (filename (buffer-file-name))
         (buf (knessy--utils-fresh-kubectl-buffer (s-concat verb "_" filename) t t t)))
    (when (buffer-modified-p)
      (if (y-or-n-p "Buffer was modified. Save and continue? ")
          (save-buffer)
        (user-error "Aborted")))
    (unless (y-or-n-p (format prompt-fmt knessy--context knessy--namespace))
      (user-error "Aborted"))
    (when (knessy--validate)
      (funcall kubectl-fn buf filename)
      ;; TODO (pgu, 17.05.2026): add toggles for deleting/killing buffer behaviour
      (message success-msg))))

(defun knessy-apply-arbitrary ()
  (interactive)
  (knessy--kubectl-file-sync-arbitrary
   "apply" #'knessy--kubectl-apply-file-sync
   "Applying to %s, namespace %s, continue? " "Applied!"))

(defun knessy-delete-arbitrary ()
  (interactive)
  (knessy--kubectl-file-sync-arbitrary
   "delete" #'knessy--kubectl-delete-file-sync
   "Deleting from %s, namespace %s, continue? " "Deleted!"))

(defun knessy--validate ()
  (let* ((filename (buffer-file-name))
         (buf-validate (knessy--utils-fresh-kubectl-buffer (s-concat "validate_" filename) t t t))
         (exit-code (knessy--kubectl-validate-file-sync buf-validate filename)))
    (if (not (= 0 exit-code))
        (progn
          (message (with-current-buffer buf-validate
                     (buffer-string)))
          nil)
      t)))

(defun knessy-close ()
  (interactive)
  (let* ((filename (buffer-file-name)))
    (kill-buffer)
    (delete-file filename)
    (message "Canceled!")))

(defvar knessy-yaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'knessy-apply)
    (define-key map (kbd "C-c C-k") 'knessy-close)
    map)
  "Keymap for `knessy-yaml-mode'.")

(defvar knessy-json-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'knessy-apply)
    (define-key map (kbd "C-c C-k") 'knessy-close)
    map)
  "Keymap for `knessy-json-mode'.")

;; TODO (pgu, 18.05.2026): sentinel echoes error on exit
(defun knessy-log-close ()
  (interactive)
  (kill-buffer))

(defun knessy-describe-close ()
  (interactive)
  (kill-buffer))

(defvar knessy-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'knessy-log-close)
    map)
  "Keymap for `knessy-log-mode'.")

(defvar knessy-describe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'knessy-describe-close)
    map)
  "Keymap for `knessy-describe-mode'.")

;; FIXME: no hooks run from yaml-ts-mode?.. keybindings are missing
;; also, are yaml-ts-mode and json-ts-mode standard and default now?
(define-derived-mode knessy-yaml-mode yaml-ts-mode "Knessy-YAML"
  "Mode for Knessy buffers with YAML objects.")

(define-derived-mode knessy-json-mode json-ts-mode "Knessy-JSON"
  "Mode for Knessy buffers with JSON objects.")

(define-derived-mode knessy-log-mode fundamental-mode "Knessy-Log"
  "Mode for Knessy buffers with logs."
  (read-only-mode))

(define-derived-mode knessy-describe-mode text-mode "Knessy-Desc"
  "Mode for Knessy buffers with object descriptions."
  (read-only-mode))

;; FIXME: my personal hacks
(add-hook
 'knessy-mode-hook
 (lambda ()
   (when display-line-numbers
     (display-line-numbers-mode -1))
   (when persp-mode
     (persp-add-buffer (current-buffer)))))

(comment
 (remove-hook
  'tabulated-list-revert-hook #'knessy--display-aio))

(provide 'knessy)
;;; knessy.el ends here
