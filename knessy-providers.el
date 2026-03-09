;;; knessy-providers.el --- Links from nodes to their provider pages for Knessy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pavel Gurkov

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Links from Kubernetes objects to the cloud provider's console.
;; Supported out of the box: AWS EC2, Azure VMs and Alibaba ECS.
;; Nodes without a cloud provider (bare metal) can be linked to an inventory
;; page with `knessy-metal-context-regexp', `knessy-metal-provider-id' and
;; `knessy-metal-node-url-format'.
;; Other providers can be added with `knessy-node-url-functions'.

(require 's)
(require 'ht)

(defvar knessy--context)
(defvar knessy--resource-type-str->sym)
(declare-function knessy--ids "knessy")
(declare-function knessy--id-ns "knessy")
(declare-function knessy--id-rt "knessy")
(declare-function knessy--id-name "knessy")
(declare-function knessy--kubectl-get-object-parsed-sync "knessy-kubectl")

(defcustom knessy-node-url-functions nil
  "Functions that return the provider page URL of a node.
Each is called with two arguments, NODE and CONTEXT, and returns a URL
string, or nil if it does not know the node.
NODE is a Node object parsed from JSON (nested hash tables), CONTEXT is the
kubectl context name string.
They are tried in order after the built-in AWS, Azure and Alibaba ones, until
one returns non-nil.
Use this for nodes without a cloud provider, e.g. bare metal, or for internal
inventory tools."
  :type 'hook
  :group 'knessy)

(defcustom knessy-metal-context-regexp nil
  "Regexp matching the kubectl context names of bare metal clusters.
Nodes of such clusters have no cloud `spec.providerID', so the cluster name is
the only way to tell them from the nodes of a local cluster such as k3s or kind.
Together with `knessy-metal-node-url-format' this links a metal node to its
page in an inventory tool.  Nil disables the metal provider."
  :type '(choice (const :tag "Disabled" nil) regexp)
  :group 'knessy)

(defcustom knessy-metal-provider-id nil
  "The `spec.providerID' that the nodes of bare metal clusters have.
Nil means the field is missing, or empty, which is what most bare metal
clusters have.
A string is for environments that put a fixed marker there instead, such as
\"private\"; a node then counts as metal only if its `spec.providerID' is
exactly that string.
Used only for the clusters matching `knessy-metal-context-regexp'."
  :type '(choice (const :tag "Missing" nil) string)
  :group 'knessy)

(defcustom knessy-metal-node-url-format nil
  "Format string of the inventory page URL of a bare metal node.
It must contain one \"%s\", which is replaced by the node name.
For example \"https://inventory.example.com/hosts/%s\".
Used only for the clusters matching `knessy-metal-context-regexp'.
Nil disables the metal provider."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'knessy)

(defconst knessy--provider-aws-provider-id-regexp
  "\\`aws:///\\([^/]+\\)/\\(i-[0-9a-f]+\\)\\'"
  "Regexp matching the `spec.providerID' of an AWS EC2 node.
The first group is the availability zone, the second the instance id.")

(defconst knessy--provider-alibaba-provider-id-regexp
  "\\`\\([a-z0-9-]+\\)\\.\\(i-[0-9a-z]+\\)\\'"
  "Regexp matching the `spec.providerID' of an Alibaba ECS node.
The first group is the region, the second the instance id, e.g.
\"cn-beijing.i-2zee1ko31lep2xiuj231\".")

(defconst knessy--provider-azure-provider-id-prefix "azure://"
  "Prefix of the `spec.providerID' of an Azure VM node.
The rest is the ARM resource id of the VM, starting with \"/subscriptions\".")

(defun knessy--provider-aws-url (node)
  "Return the AWS console URL of the EC2 instance of NODE, or nil.
NODE is a Node object parsed from JSON (nested hash tables).
The instance id and zone come from `spec.providerID', which looks like
aws:///ZONE/INSTANCE-ID.
The region comes from the `topology.kubernetes.io/region' label.
Without the label it is derived from the zone by dropping the zone letter,
which is wrong for AWS Local Zones."
  (let ((provider-id (ht-get* node "spec" "providerID")))
    (when (and provider-id
               (string-match knessy--provider-aws-provider-id-regexp provider-id))
      (let* ((zone (match-string 1 provider-id))
             (instance-id (match-string 2 provider-id))
             (region (or (ht-get* node "metadata" "labels" "topology.kubernetes.io/region")
                         (s-chop-suffix (substring zone -1) zone))))
        (format "https://%s.console.aws.amazon.com/ec2/home?region=%s#InstanceDetails:instanceId=%s"
                region region instance-id)))))

(defun knessy--provider-azure-url (node)
  "Return the Azure portal URL of the VM of NODE, or nil.
NODE is a Node object parsed from JSON (nested hash tables).
The ARM resource id of the VM is in `spec.providerID', after \"azure://\".
The portal opens any resource by its id, so scale set VMs work too."
  (let ((provider-id (ht-get* node "spec" "providerID")))
    (when (and provider-id
               (s-starts-with? knessy--provider-azure-provider-id-prefix provider-id))
      (concat "https://portal.azure.com/#@/resource"
              (s-chop-prefix knessy--provider-azure-provider-id-prefix provider-id)))))

(defun knessy--provider-alibaba-url (node)
  "Return the Alibaba console URL of the ECS instance of NODE, or nil.
NODE is a Node object parsed from JSON (nested hash tables).
The region and instance id come from `spec.providerID', which looks like
REGION.INSTANCE-ID."
  (let ((provider-id (ht-get* node "spec" "providerID")))
    (when (and provider-id
               (string-match knessy--provider-alibaba-provider-id-regexp provider-id))
      (format "https://ecs.console.aliyun.com/server/%s/detail?regionId=%s"
              (match-string 2 provider-id) (match-string 1 provider-id)))))

(defun knessy--provider-metal-url (node context)
  "Return the inventory page URL of NODE if it is a metal node, or nil.
NODE is a Node object parsed from JSON (nested hash tables).
CONTEXT is the kubectl context name string.
A metal node has the `spec.providerID' described by `knessy-metal-provider-id'
\(none by default) and CONTEXT matches `knessy-metal-context-regexp'.
The URL is `knessy-metal-node-url-format' with the node name filled in.
Always nil when either option is nil."
  (let ((provider-id (ht-get* node "spec" "providerID"))
        (name (ht-get* node "metadata" "name")))
    (when (and name
               context
               knessy-metal-context-regexp
               knessy-metal-node-url-format
               (if knessy-metal-provider-id
                   (equal provider-id knessy-metal-provider-id)
                 (or (null provider-id) (s-blank? provider-id)))
               (string-match-p knessy-metal-context-regexp context))
      (format knessy-metal-node-url-format name))))

(defun knessy--provider-node-url (node context)
  "Return the provider page URL for NODE, or nil if it is not supported.
NODE is a Node object parsed from JSON (nested hash tables).
CONTEXT is the kubectl context name string.
Tries AWS, Azure, Alibaba and metal in turn, see `knessy--provider-aws-url'
and friends, then the functions in `knessy-node-url-functions'."
  (or (knessy--provider-aws-url node)
      (knessy--provider-azure-url node)
      (knessy--provider-alibaba-url node)
      (knessy--provider-metal-url node context)
      (run-hook-with-args-until-success 'knessy-node-url-functions node context)))

(defun knessy--provider-node-name-at-point ()
  "Return the name of the node the entry at point refers to.
For a node it is the node itself, for a pod it is the node the pod runs on
\(`spec.nodeName'), which costs one kubectl call.
Reads `knessy--context' and the entry from `knessy--ids'.
Signals a `user-error' for other resource types, or for a pod that has not
been scheduled yet."
  (let* ((id (car (knessy--ids)))
         (rt-sym (ht-get knessy--resource-type-str->sym (knessy--id-rt id))))
    (pcase rt-sym
      ('node (knessy--id-name id))
      ('pod (or (ht-get* (knessy--kubectl-get-object-parsed-sync
                          (knessy--id-ns id) (knessy--id-rt id) (knessy--id-name id))
                         "spec" "nodeName")
                (user-error "Pod is not scheduled on a node yet")))
      (_ (user-error "Can only open nodes, or pods (via their node)")))))

(defun knessy-open-node-in-provider ()
  "Open the provider page of the node at point in a browser.
On a pod, open the page of the node it runs on.
AWS, Azure and Alibaba nodes open their VM in the cloud console, metal nodes
their inventory page, see `knessy-metal-node-url-format'.
Other nodes are handled by `knessy-node-url-functions'.
Signals a `user-error' if the node has no recognised provider."
  (interactive)
  (let* ((name (knessy--provider-node-name-at-point))
         ;; Nodes are cluster-scoped, so there is no namespace.
         (node (knessy--kubectl-get-object-parsed-sync nil "nodes" name))
         (url (knessy--provider-node-url node knessy--context)))
    (unless url
      (user-error "Node %s has no supported provider (providerID: %s)"
                  name (or (ht-get* node "spec" "providerID") "none")))
    (message "Opening %s" url)
    (browse-url url)))

(provide 'knessy-providers)

;;; knessy-providers.el ends here
