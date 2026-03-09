;;; knessy-options.el --- Options shared by all Knessy files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pavel Gurkov

;; The options here are used while the other files are being loaded, so they
;; cannot live in knessy.el, which requires those files.
;; Putting them in a file of their own lets `knessy.el' and the rest be
;; byte-compiled.

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

;;; Code:

(defgroup knessy nil "Customisation group for Knessy."
  :group 'extensions)

(defcustom knessy-default-view-string "default"
  "String depicting default view for a resource-type."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-namespaced-resource-types-string "*ALL-NS*"
  "String depicting all namespaced resource types.

When the resource-type is set to this value, Knessy queries every
namespaced resource type known to the cluster and shows namespace,
name and kind for each one, instead of the columns of a single
resource type."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-cluster-resource-types-string "*ALL-CLUSTER*"
  "String depicting all cluster-scoped (non-namespaced) resource types.

When the resource-type is set to this value, Knessy queries every
cluster-scoped resource type known to the cluster and shows name and
kind for each one (there's no NAMESPACE column, since it would always
be empty), instead of the columns of a single resource type."
  :type 'string
  :group 'knessy)

(defcustom knessy-linked-resource-type-string "*LINKED*"
  "String depicting the pseudo resource-type of the linked resources view.

When the resource-type is set to this value, Knessy lists the resources
linked (by ownership or references) to `knessy--linked-root', instead
of the resources of a single type."
  :type 'string
  :group 'knessy)

(defcustom knessy-all-namespaces-string "*ALL*"
  "String depicting all namespaces."
  :type 'string
  :group 'knessy)

(provide 'knessy-options)

;;; knessy-options.el ends here
