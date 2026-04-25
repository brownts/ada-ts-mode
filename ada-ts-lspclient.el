;;; ada-ts-lspclient.el --- LSP client interface -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Troy Brown

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-generic)

;;;; Customization

(defgroup ada-ts-lspclient nil
  "LSP-specific customization group for `ada-ts-mode'."
  :group 'ada-ts
  :link '(emacs-library-link :tag "Source" "ada-ts-lspclient.el")
  :link '(custom-manual "(ada-ts-mode)LSP Client Support")
  :prefix "ada-ts-lspclient-")

;;;; Setup

(defvar ada-ts-lspclient-find-functions nil
  "Special hook to find the LSP client for a given buffer.

Each function on this hook is called in turn and should return either
nil to mean that it is not applicable, or a client instance.  The exact
form of the client instance is up to each respective function; the only
practical limitation is to use values that `cl-defmethod' can dispatch
on.")

(defvar ada-ts-lspclient-session-hook nil
  "Hook called when an LSP session is established.")

(defvar ada-ts-lspclient-setup-hook nil
  "LSP client hooks to run when major mode is setup.")

;;;; LSP Client Interface

(defun ada-ts-lspclient-current ()
  "Return the client instance for the current buffer."
  (run-hook-with-args-until-success
   'ada-ts-lspclient-find-functions))

(cl-defgeneric ada-ts-lspclient-command-execute (_client command &rest arguments)
  "Execute COMMAND with ARGUMENTS using Language Server.")

(cl-defgeneric ada-ts-lspclient-command-supported-p (_client command)
  "Determine if Language Server supports COMMAND.")

(cl-defgeneric ada-ts-lspclient-document-id (_client)
  "Determine document identifier of current buffer.")

(cl-defgeneric ada-ts-lspclient-format-region (_client beg end)
  "Format region BEG to END using Language Server.")

(cl-defgeneric ada-ts-lspclient-workspace-configuration (_client scope &optional false)
  "Retrieve workspace configuration for SCOPE.

SCOPE may represent a section name (or \\='.\\=' separated nested
section name).  Additionally, the last element of scope may also be a
field name.  When scope's final element is a section, the section is
returned as a property list, whose keys are case-preserved keywords.
When scope's final element is a field, the value of the field is
returned.  When scope does not represent an existing section or field in
the configuration, nil is returned.

If FALSE is provided, it represents the JSON false value in Emacs Lisp.
When omitted, it is represented as nil.")

(cl-defgeneric ada-ts-lspclient-workspace-dirs-add (_client dirs)
  "Add workspace DIRS to session.")

(cl-defgeneric ada-ts-lspclient-workspace-root (_client path)
  "Determine workspace root for PATH.")

(provide 'ada-ts-lspclient)

;;; ada-ts-lspclient.el ends here
