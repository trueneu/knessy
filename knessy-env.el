;; -*- lexical-binding: t; -*-

(defun knessy--env ()
  "Return the current env of this buffer as a list.
The elements are, in order: `knessy-kubeconfig', `knessy--context',
`knessy--namespace', `knessy--resource-type', `knessy--label-selectors',
`knessy--field-selectors', `knessy--view' and `knessy--linked-root'.
Two envs are compared with `equal'."
  (list knessy-kubeconfig knessy--context knessy--namespace knessy--resource-type knessy--label-selectors knessy--field-selectors knessy--view knessy--linked-root))

(defun knessy--linked-root->str (root &optional short-context)
  "Format linked ROOT, a (namespace resource-type name) list, as ns/type/name.
Nil components are left out, so a cluster-scoped ROOT has no namespace part.
SHORT-CONTEXT is optional.
If it is non-nil, it is a context name, and the resource type is shortened for
display using the api-resources of that context; see
`knessy--resource-type-short'.
Return the resulting string."
  (pcase-let ((`(,ns ,rt ,name) root))
    (s-join "/" (delq nil (list ns
                                (if (and rt short-context)
                                    (knessy--resource-type-short rt short-context)
                                  rt)
                                name)))))

(defun knessy--env-entry->str (env)
  "Format the env ENV, as returned by `knessy--env', as a string.
The elements of ENV (config, context, namespace, resource type, labels,
fields, view and linked root) are joined with colons.
The label and field selectors are formatted with `knessy--utils-alist->str-='
and the linked root with `knessy--linked-root->str'."
  (let* ((config (nth 0 env))
         (ctx (nth 1 env))
         (ns (nth 2 env))
         (rt (nth 3 env))
         (labels (nth 4 env))
         (fields (nth 5 env))
         (view (nth 6 env))
         (linked-root (nth 7 env)))
    (s-join ":" (list config ctx ns rt
                      (knessy--utils-alist->str-= labels)
                      (knessy--utils-alist->str-= fields)
                      view
                      (knessy--linked-root->str linked-root)))))


(defun knessy--set-env (env)
  "Make ENV, a list as returned by `knessy--env', the current env.
Do nothing if ENV is nil.
Sets `knessy-kubeconfig', the selectors and `knessy--linked-root' in the
current buffer, then applies the context, namespace, resource type and view
with `knessy--set-context', `knessy--set-namespace',
`knessy--set-resource-type' and `knessy--set-view'.
These also update the last-selected values, and may reset selectors
depending on the user options.
This does not redisplay anything."
  ;; if env is nil, do nothing
  (when env
    ;; TODO: maybe write first, second, etc?
    (let* ((config (nth 0 env))
           (ctx (nth 1 env))
           (ns (nth 2 env))
           (rt (nth 3 env))
           (labels (nth 4 env))
           (fields (nth 5 env))
           (view (nth 6 env)))
      (setq
       knessy-kubeconfig config
       knessy--label-selectors labels
       knessy--field-selectors fields
       knessy--linked-root (nth 7 env))
      (knessy--set-context ctx)
      (knessy--set-namespace ns)
      (knessy--set-resource-type rt)
      (knessy--set-view rt view))))


(defun knessy--env-equal-current (env)
  "Return non-nil if ENV is equal to the current env, see `knessy--env'."
  ;; might become more complicated if we don't want to include regex or other filters in equality, but want to restore them
  (equal (knessy--env) env))

(defvar knessy--env-ring-length 32)
(defvar-local knessy--env-ring (make-vector knessy--env-ring-length nil))
(defvar-local knessy--env-ring-begin 0)
(defvar-local knessy--env-ring-read 0)
(defvar-local knessy--env-ring-write 1)
(defvar-local knessy--env-ring-end 1)

(defun knessy--env-ring-reset ()
  "Reset the env ring of the current buffer to hold only the current env.
The ring is emptied, and the current env, see `knessy--env', is stored at
index 0.
The begin, read, write and end positions are reset accordingly."
  (interactive)
  (setq knessy--env-ring (make-vector knessy--env-ring-length nil)
        knessy--env-ring-begin 0
        knessy--env-ring-read 0
        knessy--env-ring-write 1
        knessy--env-ring-end 1)
  (aset knessy--env-ring 0 (knessy--env)))

(defun knessy--env-ring-show ()
  "Show the raw env ring of the current buffer, for debugging.
The ring is listed in the *knessy-env-ring* buffer, which is displayed.
Each slot of the ring is shown with markers for the begin, end, read and
write positions."
  (interactive)
  (let* ((buf (get-buffer-create "*knessy-env-ring*"))
         (items (mapcar
                 #'knessy--env-ring-tablist-item
                 (number-sequence 0 (1- knessy--env-ring-length))))
         (entries (mapcar
                   (lambda (vec) (list nil vec))
                   items)))
    (with-current-buffer buf
      (tabulated-list-mode)
      (setq tabulated-list-format '[("begin" 5 t)
                                    ("end" 5 t)
                                    ("read" 5 t)
                                    ("write" 5 t)
                                    ("item" 50 t)])
      (setq tabulated-list-entries entries)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buf)))

(defun knessy--env-ring-show-pretty ()
  "Show the env ring of the current buffer in a more compact form.
The ring is listed in the *knessy-env-ring* buffer, which is displayed.
The begin and end slots are marked with \">>>\" and \"<<<\", and the read
slot with \"*\"."
  (interactive)
  (let* ((buf (get-buffer-create "*knessy-env-ring*"))
         (items (mapcar
                 #'knessy--env-ring-tablist-item-pretty
                 (number-sequence 0 (1- knessy--env-ring-length))))
         (entries (mapcar
                   (lambda (vec) (list nil vec))
                   items)))
    (with-current-buffer buf
      (tabulated-list-mode)
      (setq tabulated-list-format '[("POSITION" 10 t)
                                    ("ITEM" 50 t)])
      (setq tabulated-list-entries entries)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buf)))


(defun knessy--env-ring-tablist-item (idx)
  "Return a vector of table cells describing the ring slot IDX.
The first four cells are \"*\" if IDX is the begin, end, read or write
position respectively, and \"\" otherwise.
The last cell is the env stored in the slot, formatted with
`knessy--env-entry->str'."
  (vector
   (if (= idx knessy--env-ring-begin) "*" "")
   (if (= idx knessy--env-ring-end) "*" "")
   (if (= idx knessy--env-ring-read) "*" "")
   (if (= idx knessy--env-ring-write) "*" "")
   (knessy--env-entry->str (aref knessy--env-ring idx))))

(defun knessy--env-ring-tablist-item-pretty (idx)
  "Return a vector of two table cells describing the ring slot IDX.
The first cell holds \">>> \" if IDX is the begin position, \"<<< \" if it is
the end position, and \"*\" if it is the read position.
The second cell is the env stored in the slot, formatted with
`knessy--env-entry->str'."
  (interactive)
  (vector
   (s-concat
    (if (= idx knessy--env-ring-begin) ">>> " "")
    (if (= idx knessy--env-ring-end) "<<< " "")
    (if (= idx knessy--env-ring-read) "*" ""))
   (knessy--env-entry->str (aref knessy--env-ring idx))))

(defun knessy--env-ring-init ()
  "Store the current env, see `knessy--env', at index 0 of the env ring.
Other ring positions are not changed."
  (aset knessy--env-ring 0 (knessy--env)))

(defun knessy--env-ring-dec (v)
  "Return the ring index before V, wrapping around the ring length."
  (mod (+ (1- v) knessy--env-ring-length)
       knessy--env-ring-length))

(defun knessy--env-ring-inc (v)
  "Return the ring index after V, wrapping around the ring length."
  (mod (+ (1+ v) knessy--env-ring-length)
       knessy--env-ring-length))

(defun knessy--env-ring-read-back ()
  "Move the read position of the env ring one entry back, and return that env.
Stay in place if the read position is already at the beginning of the ring.
The write position is set to follow the read position.
Return the env at the read position after the move.
This does not apply the env."
  (unless (= knessy--env-ring-read knessy--env-ring-begin)
    (let* ((new-read (knessy--env-ring-dec knessy--env-ring-read))
           (new-write (knessy--env-ring-inc new-read)))
      (setq knessy--env-ring-read new-read
            knessy--env-ring-write new-write)))
  (aref knessy--env-ring knessy--env-ring-read))

(defun knessy--env-ring-read-forward ()
  "Move the read position of the env ring one entry forward, and return that env.
Stay in place if the read position is already at the newest entry.
The write position is set to follow the read position, if it moved.
Return the env at the read position after the move.
This does not apply the env."
  (let* ((new-read (knessy--env-ring-inc knessy--env-ring-read))
         (new-write (knessy--env-ring-inc new-read)))
    (unless (= new-read knessy--env-ring-end)
      (setq knessy--env-ring-read new-read
            knessy--env-ring-write new-write)))
  (aref knessy--env-ring knessy--env-ring-read))

(defun knessy--env-ring-peek ()
  "Return the env at the read position of the env ring, without moving."
  (aref knessy--env-ring knessy--env-ring-read))

(defun knessy--env-ring-push (v)
  "Push the env V onto the env ring, making it the current entry.
Do nothing if V is `equal' to the entry at the read position.
Otherwise V is stored at the write position, the read position moves to it,
and the entries after it, if any, are discarded.
If the ring is full, the oldest entry is overwritten.
Return value is not meaningful."
  (unless (equal (knessy--env-ring-peek) v)
    (let* ((new-read knessy--env-ring-write)
           (new-write (knessy--env-ring-inc knessy--env-ring-write))
           (new-end new-write))
      (aset knessy--env-ring knessy--env-ring-write v)
      (when (and (= knessy--env-ring-write knessy--env-ring-begin))
        (setq knessy--env-ring-begin (knessy--env-ring-inc knessy--env-ring-begin)))
      (setq knessy--env-ring-read new-read
            knessy--env-ring-write new-write
            knessy--env-ring-end new-end))))

(defun knessy--env-ring-live-indices ()
  "Return ring indices with valid entries, oldest first.
Operates on the env ring of the current buffer.
Walks from `knessy--env-ring-begin' up to but not including
`knessy--env-ring-end' (wrapping around)."
  (let ((indices (list knessy--env-ring-begin))
        (i (knessy--env-ring-inc knessy--env-ring-begin)))
    (while (/= i knessy--env-ring-end)
      (push i indices)
      (setq i (knessy--env-ring-inc i)))
    (nreverse indices)))

(defvar-local knessy--env-history-origin-buffer nil
  "Knessy buffer whose env ring is shown in this history view.")

(defvar knessy-env-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'knessy-env-history-jump)
    map)
  "Keymap for `knessy-env-history-mode'.")

(define-derived-mode knessy-env-history-mode tabulated-list-mode "Knessy/EnvHist"
  "Major mode for browsing the knessy environment history ring."
  (setq tabulated-list-format [("CUR" 4 nil)
                               ("CONFIG" 30 nil)
                               ("CTX" 15 nil)
                               ("NS" 20 nil)
                               ("RES-TYPE" 20 nil)
                               ("LABELS" 30 nil)
                               ("FIELDS" 30 nil)
                               ("VIEW" 30 nil)
                               ("LINKED" 40 nil)])
  (tabulated-list-init-header))

(defun knessy-env-history ()
  "Show the env ring history; RET on an entry jumps to it.
Most-recent entries appear first. Selecting an entry sets the env in the
originating knessy buffer, updates the ring's read position, and redisplays.
Must be called in a Knessy buffer, which becomes the origin buffer of the
history view.
The history is shown in the *knessy-env-history* buffer, which is selected
with `pop-to-buffer'.
The entry at the current read position is marked with \"*\"."
  (interactive)
  (let* ((origin (current-buffer))
         (current-idx knessy--env-ring-read)
         (ring knessy--env-ring)
         (indices (nreverse (knessy--env-ring-live-indices)))
         (entries (mapcar
                   (lambda (idx)
                     (let* ((entry (aref ring idx)))
                       (list idx
                             (vector
                              (if (= idx current-idx) "*" "")
                              ;; TODO (pgu, 23.05.2026): destructure this properly
                              (or (nth 0 entry) "")
                              (or (nth 1 entry) "")
                              (or (nth 2 entry) "")
                              (knessy--resource-type-short (or (nth 3 entry) "") (nth 1 entry))
                              (knessy--utils-alist->str-= (nth 4 entry))
                              (knessy--utils-alist->str-= (nth 5 entry))
                              (or (nth 6 entry) "")
                              ;; the root is only meaningful for the linked view
                              (if (equal (nth 3 entry) knessy-linked-resource-type-string)
                                  (knessy--linked-root->str (nth 7 entry) (nth 1 entry))
                                "")))))
                   indices))
         (buf (get-buffer-create "*knessy-env-history*")))
    (with-current-buffer buf
      (knessy-env-history-mode)
      (setq knessy--env-history-origin-buffer origin)
      (setq tabulated-list-entries entries)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun knessy-env-history-jump ()
  "Set the env in the origin buffer to the entry under point and redisplay.
Must be called in a `knessy-env-history-mode' buffer.
The read position of the origin buffer's ring is set to the entry, and the
write position after it; then the env is applied with `knessy--set-env' and
the buffer is redrawn with `knessy--display2'.
The origin buffer is selected and the history buffer is killed.
Signals a `user-error' if there is no entry on the line or the origin buffer
is gone."
  (interactive)
  (let ((idx (tabulated-list-get-id))
        (origin knessy--env-history-origin-buffer)
        (hist-buf (current-buffer)))
    (unless idx (user-error "No entry on this line"))
    (unless (buffer-live-p origin) (user-error "Origin buffer is gone"))
    (with-current-buffer origin
      (setq knessy--env-ring-read idx
            knessy--env-ring-write (knessy--env-ring-inc idx))
      (knessy--set-env (aref knessy--env-ring idx))
      (knessy--display2))
    (pop-to-buffer origin)
    (kill-buffer hist-buf)))

(comment
 (knessy--env-ring-reset)
 (knessy--env-ring-show)
 (knessy--env-ring-read-back)
 (knessy--env-ring-read-forward)
 (knessy--env-ring-push (list "1"))
 (knessy--env-ring-push (list "2"))
 (knessy--env-ring-push (list "3"))
 (knessy--env-ring-push (list "4"))
 (knessy--env-ring-push (list "5"))
 (knessy--env-ring-push (list "6")))

(comment
 (knessy--env-append)
 (knessy--env-pop)
 (knessy--set-env (knessy--env-pop))
 knessy--env-history)

;; TODO: add config here?
(defun knessy--set-env-last-selected ()
  "Set the current context, namespace and resource type to the last selected.
Each of `knessy--context', `knessy--namespace' and `knessy--resource-type' is
set from its `knessy--last-selected-...' counterpart, if that is non-nil.
Nothing else is touched, including the kubeconfig, selectors and view.
Return value is not meaningful."
  (when knessy--last-selected-context
    (setq knessy--context knessy--last-selected-context))
  (when knessy--last-selected-namespace
    (setq knessy--namespace knessy--last-selected-namespace))
  (when knessy--last-selected-resource-type
    (setq knessy--resource-type knessy--last-selected-resource-type)))

(provide 'knessy-env)
