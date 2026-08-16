;;; ada-ts-common.el -- Common support for GPR Project files -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Troy Brown

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

(require 'lisp-mnt)
(require 'treesit)

;;;; Customization

(defgroup ada-ts nil
  "Major mode for Ada, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "ada-ts-mode.el")
  :link `(url-link :tag "Website"
                   ,(lm-website (locate-library "ada-ts-mode.el")))
  :link '(custom-manual "(ada-ts-mode)Top")
  :prefix "ada-ts-mode-")

;;;; Private Hooks

;;;; Keywords

(defvar ada-ts-mode--keywords
  '("abort" "abstract" "accept" "access" "aliased" "all" "and" "array" "at"
    "begin" "body"
    "case" "constant"
    "declare" "delay" "delta" "digits" "do"
    "else" "elsif" "end" "entry" "exception" "exit"
    "for" "function"
    "generic" "goto"
    "if" "in" "interface" "is"
    "limited" "loop"
    "mod"
    "new" "not" "null"
    "of" "or" "others" "out" "overriding"
    "package" "parallel" "pragma" "private" "procedure" "protected"
    "raise" "range" "record" "renames" "requeue" "return" "reverse"
    "select" "separate" "some" "subtype" "synchronized"
    "tagged" "task" "terminate" "then" "type"
    "until" "use"
    "when" "while" "with")
  "Ada keywords for tree-sitter font-locking.")

;;;; Support Macros

(defmacro ada-ts-mode--declare-treesit-functions ()
  "Declare C functions defined in treesit.c.

This macro is only needed when a file needs to be able to byte-compile
in an Emacs not built with tree-sitter library."
  (if (fboundp 'treesit-declare-unavailable-functions)
      (treesit-declare-unavailable-functions)
    '(progn
       (declare-function treesit-compiled-query-p "treesit.c")
       (declare-function treesit-induce-sparse-tree "treesit.c")
       (declare-function treesit-language-available-p "treesit.c")
       (declare-function treesit-parser-create "treesit.c")
       (declare-function treesit-node-check "treesit.c")
       (declare-function treesit-node-child "treesit.c")
       (declare-function treesit-node-child-by-field-name "treesit.c")
       (declare-function treesit-node-child-count "treesit.c")
       (declare-function treesit-node-end "treesit.c")
       (declare-function treesit-node-eq "treesit.c")
       (declare-function treesit-node-next-sibling "treesit.c")
       (declare-function treesit-node-p "treesit.c")
       (declare-function treesit-node-parent "treesit.c")
       (declare-function treesit-node-prev-sibling "treesit.c")
       (declare-function treesit-node-start "treesit.c")
       (declare-function treesit-node-type "treesit.c")
       (declare-function treesit-query-compile "treesit.c")
       (declare-function treesit-query-expand "treesit.c")
       (declare-function treesit-search-subtree "treesit.c"))))

(ada-ts-mode--declare-treesit-functions)

;;;; Node Predicates

(defun ada-ts-mode--package-p (node)
  "Determine if NODE is a package declaration, body or stub.
Return non-nil to indicate that it is."
  (pcase (treesit-node-type node)
    ((or "generic_instantiation"
         "generic_renaming_declaration")
     (treesit-filter-child
      node
      (lambda (n)
        (let ((node-type (treesit-node-type n)))
          (string-equal "package" node-type)))))
    ("package_declaration"
     (not (string-equal "generic_package_declaration"
                        (treesit-node-type (treesit-node-parent node)))))
    ((or "formal_package_declaration"
         "generic_package_declaration"
         "package_body"
         "package_body_stub"
         "package_renaming_declaration")
     t)))

(defun ada-ts-mode--subprogram-p (node)
  "Determine if NODE is a subprogram declaration, body or stub.
Return non-nil to indicate that it is."
  (pcase (treesit-node-type node)
    ((or "generic_instantiation"
         "generic_renaming_declaration")
     (treesit-filter-child
      node
      (lambda (n)
        (let ((node-type (treesit-node-type n)))
          (or (string-equal "function" node-type)
              (string-equal "procedure" node-type))))))
    ((or "expression_function_declaration"
         "formal_abstract_subprogram_declaration"
         "formal_concrete_subprogram_declaration"
         "generic_subprogram_declaration"
         "null_procedure_declaration"
         "subprogram_body"
         "subprogram_body_stub"
         "subprogram_declaration"
         "subprogram_renaming_declaration")
     t)))

(defun ada-ts-mode--protected-p (node)
  "Determine if NODE is a protected declaration, body, body stub or type."
  (pcase (treesit-node-type node)
    ((or "protected_body"
         "protected_body_stub"
         "protected_type_declaration"
         "single_protected_declaration")
     t)))

(defun ada-ts-mode--task-p (node)
  "Determine if NODE is a task declaration, body, body stub type."
  (pcase (treesit-node-type node)
    ((or "single_task_declaration"
         "task_body"
         "task_body_stub"
         "task_type_declaration")
     t)))

(defun ada-ts-mode--type-declaration-p (node)
  "Determine if NODE is a type declaration."
  (pcase (treesit-node-type node)
    ((or "formal_complete_type_declaration"
         "formal_incomplete_type_declaration"
         "incomplete_type_declaration"
         "private_extension_declaration"
         "private_type_declaration"
         "protected_type_declaration"
         "task_type_declaration"
         "subtype_declaration")
     t)
    ("full_type_declaration"
     (let ((child (treesit-node-type (treesit-node-child node 0))))
       (and (not (string-equal child "task_type_declaration"))
            (not (string-equal child "protected_type_declaration")))))))

(defun ada-ts-mode--with-clause-name-p (node)
  "Determine if NODE is a library unit name within a with clause."
  (and (string-equal (treesit-node-type (treesit-node-parent node))
                     "with_clause")
       (pcase (treesit-node-type node)
         ((or "identifier"
              "selected_component")
          t))))

(defun ada-ts-mode--defun-p (node)
  "Determine if NODE is candidate for defun."
  (let ((type (treesit-node-type node)))
    (and type
         (string-match (car treesit-defun-type-regexp) type)
         (pcase type
           ("package_declaration"
            (not (string-equal "generic_package_declaration"
                               (treesit-node-type (treesit-node-parent node)))))
           (_ t)))))

;;;; Node Name Utilities

(defun ada-ts-mode--adjust-text-properties (value)
  "Adjust text properties of VALUE string for use outside the buffer.

Replace faces in VALUE text properties which are locally remapped.  The
face is substituted with its replacement from `face-remapping-alist'
allowing the string to be displayed with the same attributes outside of
the current buffer.

When no face is specified, use the default foreground face.  This can be
helpful when VALUE is displayed outside the buffer with some other
default foreground face.

When the default face is specified, use only the foreground attribute
from the face.  This can be helpful when VALUE is displayed outside the
buffer where use of the default face's background attribute could
interfere with other display mechanisms, such as selection highlighting.

All non-face text properties are stripped from VALUE."
  (letrec ((new-value (substring-no-properties value))
           (len (length new-value))
           (pos 0)
           (adjust-face
            (lambda (face)
              (cond
               ;; List of faces
               ((and (consp face)
                     (not (keywordp (car face))))
                (seq-map adjust-face face))
               ;; No face
               ((null face)
                (list :foreground (face-foreground 'default)))
               ;; Remapped face
               ((and (symbolp face)
                     (buffer-local-boundp 'face-remapping-alist (current-buffer))
                     (alist-get face face-remapping-alist)))
               ;; Default face
               ((and (symbolp face)
                     (eq face 'default))
                (list :foreground (face-foreground 'default)))
               ;; Everything else
               (t face)))))
    (while (< pos len)
      (let* ((next (next-single-property-change pos 'face value len))
             (face (get-text-property pos 'face value))
             (modified-face (funcall adjust-face face)))
        (put-text-property pos next 'face modified-face new-value)
        (setq pos next)))
    new-value))

(defun ada-ts-mode--node-to-name (node &optional no-property)
  "Return value of NODE as a name string.

If optional argument NO-PROPERTY is non-nil, remove text properties."
  (pcase (treesit-node-type node)
    ((or "identifier" "string_literal")
     (if no-property
         (treesit-node-text node 'no-property)
       (font-lock-ensure (treesit-node-start node) (treesit-node-end node))
       (ada-ts-mode--adjust-text-properties (treesit-node-text node))))
    ("selected_component"
     (string-join
      (append (ensure-list (ada-ts-mode--node-to-name
                            (treesit-node-child-by-field-name node "prefix")
                            no-property))
              (list (ada-ts-mode--node-to-name
                     (treesit-node-child-by-field-name node "selector_name")
                     no-property)))
      treesit-add-log-defun-delimiter))))

;;;; Declaration Names

(defun ada-ts-mode--type-declaration-name (node)
  "Return the type declaration name of NODE."
  (ada-ts-mode--node-to-name
   (car (treesit-filter-child
         node
         (lambda (n)
           (string-equal (treesit-node-type n)
                         "identifier"))))))

;;;; Defun Names

(defun ada-ts-mode--defun-name (node &optional no-property)
  "Return the defun name of NODE.

Return nil if there is no name or if NODE is not a defun node.

If optional argument NO-PROPERTY is non-nil, remove text properties."
  (ada-ts-mode--node-to-name
   (pcase (treesit-node-type node)
     ((or "expression_function_declaration"
          "formal_abstract_subprogram_declaration"
          "formal_concrete_subprogram_declaration"
          "generic_subprogram_declaration"
          "null_procedure_declaration"
          "subprogram_body"
          "subprogram_body_stub"
          "subprogram_declaration"
          "subprogram_renaming_declaration")
      (treesit-node-child-by-field-name
       (car (treesit-filter-child
             node
             (lambda (n)
               (pcase (treesit-node-type n)
                 ((or "function_specification"
                      "procedure_specification")
                  t)
                 (_ nil)))))
       "name"))
     ("generic_package_declaration"
      (treesit-node-child-by-field-name
       (car (treesit-filter-child
             node
             (lambda (n)
               (string-equal "package_declaration"
                             (treesit-node-type n)))))
       "name"))
     ("package_declaration"
      (when (not (string-equal "generic_package_declaration"
                               (treesit-node-type (treesit-node-parent node))))
        (treesit-node-child-by-field-name node "name")))
     ((or "generic_instantiation"
          "package_body"
          "package_renaming_declaration")
      (treesit-node-child-by-field-name node "name"))
     ("generic_renaming_declaration"
      (treesit-node-child-by-field-name node "defining_program_unit_name"))
     ((or "entry_body"
          "entry_declaration"
          "formal_package_declaration"
          "package_body_stub"
          "protected_body"
          "protected_body_stub"
          "protected_type_declaration"
          "single_protected_declaration"
          "single_task_declaration"
          "task_body"
          "task_body_stub"
          "task_type_declaration")
      (car (treesit-filter-child
            node
            (lambda (n)
              (let ((node-type (treesit-node-type n)))
                (string-equal "identifier" node-type))))))
     ("subunit"
      (treesit-node-child-by-field-name node "parent_unit_name")))
   no-property))

(provide 'ada-ts-common)

;;; ada-ts-common.el ends here
