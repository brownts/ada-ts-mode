;;; ada-ts-mode.el --- Major mode for Ada using Tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Troy Brown

;; Author: Troy Brown <brownts@troybrown.dev>
;; Created: February 2023
;; Version: 0.9.0snapshot
;; Keywords: ada languages tree-sitter
;; URL: https://github.com/brownts/ada-ts-mode
;; Package-Requires: ((emacs "29.1"))

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

;; This package provides Ada syntax highlighting and navigation using
;; Tree-Sitter.  To use the `ada-ts-mode' major mode you will need the
;; appropriate grammar installed.  By default, on mode startup if the
;; grammar is not detected, you will be prompted to automatically
;; install it.

;;; Code:

(require 'ada-ts-als)
(require 'ada-ts-casing)
(require 'ada-ts-common)
(require 'ada-ts-imenu)
(require 'ada-ts-indentation)
(require 'ada-ts-lspclient)
(require 'find-file)
(require 'lisp-mnt)
(require 'project)
(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

;;; Customization

(defcustom ada-ts-mode-alire-program "alr"
  "Name of Alire executable program."
  :type 'string
  :risky t
  :group 'ada-ts
  :link '(url-link :tag "Alire Website" "https://alire.ada.dev/")
  :package-version '(ada-ts-mode . "0.7.0"))

(defcustom ada-ts-mode-grammar "https://github.com/briot/tree-sitter-ada"
  "Configuration for downloading and installing the tree-sitter language grammar.

Additional settings beyond the git repository can also be
specified.  See `treesit-language-source-alist' for full details."
  :type '(choice (string :tag "Git Repository")
                 (list :tag "All Options"
                       (string :tag "Git Repository")
                       (choice :tag "Revision" (const :tag "Default" nil) string)
                       (choice :tag "Source Directory" (const :tag "Default" nil) string)
                       (choice :tag "C Compiler" (const :tag "Default" nil) string)
                       (choice :tag "C++ Compiler" (const :tag "Default" nil) string)))
  :group 'ada-ts
  :link '(custom-manual :tag "Grammar Installation" "(ada-ts-mode)Grammar Installation")
  :package-version '(ada-ts-mode . "0.5.0"))

(defcustom ada-ts-mode-grammar-install 'prompt
  "Configuration for installation of tree-sitter language grammar library."
  :type '(choice (const :tag "Automatically Install" auto)
                 (const :tag "Prompt to Install" prompt)
                 (const :tag "Do not install" nil))
  :group 'ada-ts
  :link '(custom-manual :tag "Grammar Installation" "(ada-ts-mode)Grammar Installation")
  :package-version '(ada-ts-mode . "0.5.0"))

(defcustom ada-ts-mode-keymap-prefix "C-c"
  "Keymap prefix for `ada-ts-mode'."
  :type 'string
  :group 'ada-ts
  :link '(custom-manual :tag "Miscellaneous" "(ada-ts-mode)Miscellaneous")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-other-file-alist
  `((,(rx   ".ads" eos) (  ".adb"))
    (,(rx   ".ADS" eos) (  ".ADB"))
    (,(rx   ".adb" eos) (  ".ads"))
    (,(rx   ".ADB" eos) (  ".ADS"))
    (,(rx ".1.ada" eos) (".2.ada"))
    (,(rx ".1.ADA" eos) (".2.ADA"))
    (,(rx ".2.ada" eos) (".1.ada"))
    (,(rx ".2.ADA" eos) (".1.ADA"))
    (,(rx  "_.ada" eos) (  ".ada"))
    (,(rx  "_.ADA" eos) (  ".ADA"))
    (,(rx   ".ada" eos) ( "_.ada"))
    (,(rx   ".ADA" eos) ( "_.ADA")))
  "Ada file extension mapping for \\='find other file\\='."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :group 'ada-ts
  :link '(custom-manual :tag "Navigation" "(ada-ts-mode)Navigation")
  :link '(function-link ff-find-other-file)
  :link '(variable-link ff-other-file-alist)
  :package-version '(ada-ts-mode . "0.9.0"))

;;; Syntax

(defvar ada-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?&  "."    table)
    (modify-syntax-entry ?\| "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?\' "."    table)
    (modify-syntax-entry ?\\ "."    table)
    (modify-syntax-entry ?\n ">"    table)
    table)
  "Syntax table for `ada-ts-mode'.")

(defun ada-ts-mode--syntax-propertize (beg end)
  "Apply syntax text property to character literals between BEG and END.

This is necessary to suppress interpreting syntactic meaning from a
chararacter literal (e.g., double-quote character incorrectly
interpreted as the beginning or end of a string).  The single-quote
character is not defined in the syntax table as a string since it is
also used with attributes.  Thus, it is defined in the syntax table as
punctuation and we identify character literal instances here and apply
the string property to those instances."
  (goto-char beg)
  (while (re-search-forward (rx "'" anychar "'") end t)
    (pcase (treesit-node-type
            (treesit-node-at (match-beginning 0)))
      ("character_literal"
       ;; (info "(elisp) Syntax Table Internals")
       (let ((descriptor (string-to-syntax "\""))
             (beginning (match-beginning 0))
             (end (match-end 0)))
         (put-text-property beginning (1+ beginning) 'syntax-table descriptor)
         (put-text-property (1- end) end 'syntax-table descriptor))))))

;;; Font Lock

(defvar ada-ts-mode--preproc-keywords
  '("#if" "#elsif" "#else" "#end" "if" "then" ";")
  "Ada preprocessor keywords for tree-sitter font-locking.")

(defvar ada-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; Assignment
   :language 'ada
   :feature 'assignment
   '((assignment_statement
      variable_name: (identifier) @font-lock-variable-use-face)
     ((assignment_statement
       variable_name: (selected_component
                       selector_name: (identifier) @font-lock-variable-use-face))
      (:match "^\\(?:[^aA]\\|[aA][^lL]\\|[aA][lL][^lL]\\|[aA][lL][lL].\\)"
              @font-lock-variable-use-face))
     (assignment_statement
      variable_name: (slice
                      prefix: (identifier) @font-lock-variable-use-face))
     ((assignment_statement
       variable_name: (slice
                       prefix: (selected_component
                                selector_name: (identifier) @font-lock-variable-use-face)))
      (:match "^\\(?:[^aA]\\|[aA][^lL]\\|[aA][lL][^lL]\\|[aA][lL][lL].\\)"
              @font-lock-variable-use-face)))

   ;; Attributes
   :language 'ada
   :feature 'attribute
   '(((attribute_designator) @font-lock-property-use-face)
     (range_attribute_designator "range" @font-lock-property-use-face)
     (reduction_attribute_designator (identifier) @font-lock-property-use-face)
     (component_declaration (identifier) @font-lock-property-name-face)
     (component_choice_list (identifier) @font-lock-property-name-face)
     (component_clause local_name: _ @font-lock-property-name-face)
     (discriminant_association (identifier) @font-lock-property-name-face))

   ;; Brackets
   :language 'ada
   :feature 'bracket
   '((["(" ")" "[" "]"]) @font-lock-bracket-face)

   ;; Comments
   :language 'ada
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; Constants
   :language 'ada
   :feature 'constant
   '(((term name: (identifier) @font-lock-constant-face)
      (:match "^\\(?:[tT][rR][uU][eE]\\|[fF][aA][lL][sS][eE]\\)$"
              @font-lock-constant-face))
     (enumeration_type_definition (identifier) @font-lock-constant-face)
     (enumeration_representation_clause
      (enumeration_aggregate
       (named_array_aggregate
        (array_component_association
         (discrete_choice_list
          (discrete_choice
           (expression
            (term name: (identifier) @font-lock-constant-face))))))))
     ((primary_null) @font-lock-constant-face))

   ;; Delimiters
   :language 'ada
   :feature 'delimiter
   '(["," "." ":" ";"] @font-lock-delimiter-face)

   ;; Definitions
   :language 'ada
   :feature 'definition
   :override 'prepend
   '((procedure_specification name: (identifier) @font-lock-function-name-face)
     (procedure_specification name: (selected_component
                                     selector_name: (identifier)
                                     @font-lock-function-name-face))
     (function_specification name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (function_specification name: (selected_component
                                    selector_name: _ @font-lock-function-name-face))
     (subprogram_body endname: [(identifier) (string_literal)] @font-lock-function-name-face)
     (subprogram_body endname: (selected_component
                                selector_name: _ @font-lock-function-name-face))
     (subprogram_default default_name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (subprogram_default default_name: (selected_component
                                        selector_name: _ @font-lock-function-name-face))
     (entry_declaration "entry"
                        :anchor (comment) :*
                        :anchor (identifier) @font-lock-function-name-face)
     (entry_body (identifier) @font-lock-function-name-face)
     (accept_statement entry_direct_name: _ @font-lock-function-name-face)
     (accept_statement entry_identifier: _ @font-lock-function-name-face)
     (single_protected_declaration "protected"
                                   :anchor (comment) :*
                                   :anchor (identifier) @font-lock-variable-name-face)
     (single_protected_declaration
      (protected_definition "end" (identifier) @font-lock-variable-name-face))
     (protected_body (identifier) @font-lock-variable-name-face)
     (protected_body_stub (identifier) @font-lock-variable-name-face)
     (single_task_declaration "task"
                              :anchor (comment) :*
                              :anchor (identifier) @font-lock-variable-name-face)
     (single_task_declaration
      (task_definition "end" (identifier) @font-lock-variable-name-face))
     (task_body (identifier) @font-lock-variable-name-face)
     (task_body_stub (identifier) @font-lock-variable-name-face)
     (generic_instantiation
      ["procedure" "function"]
      name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (generic_instantiation
      ["procedure" "function"]
      name: (selected_component
             selector_name: _ @font-lock-function-name-face))
     (generic_instantiation
      ["procedure" "function"]
      generic_name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (generic_instantiation
      ["procedure" "function"]
      generic_name: (function_call name: [(identifier) (string_literal)]
                                   @font-lock-function-name-face))
     (generic_instantiation
      ["procedure" "function"]
      generic_name: (selected_component
                     selector_name: _ @font-lock-function-name-face))
     (generic_instantiation
      ["procedure" "function"]
      generic_name: (function_call name: (selected_component
                                          selector_name: _ @font-lock-function-name-face)))
     (subprogram_renaming_declaration
      callable_entity_name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (subprogram_renaming_declaration
      callable_entity_name: (selected_component
                             selector_name: _ @font-lock-function-name-face))
     (generic_renaming_declaration
      ["procedure" "function"]
      defining_program_unit_name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (generic_renaming_declaration
      ["procedure" "function"]
      defining_program_unit_name: (selected_component
                                   selector_name: _ @font-lock-function-name-face))
     (generic_renaming_declaration
      generic_function_name: [(identifier) (string_literal)] @font-lock-function-name-face)
     (generic_renaming_declaration
      generic_function_name: (selected_component
                              selector_name: _ @font-lock-function-name-face))
     (generic_renaming_declaration
      generic_procedure_name: (identifier) @font-lock-function-name-face)
     (generic_renaming_declaration
      generic_procedure_name: (selected_component
                               selector_name: (identifier)
                               @font-lock-function-name-face))
     (object_declaration (identifier) @font-lock-variable-name-face ":")
     (object_declaration (identifier) @font-lock-constant-face ":" "constant")
     (number_declaration (identifier) @font-lock-constant-face ":")
     (extended_return_object_declaration (identifier) @font-lock-variable-name-face ":")
     (extended_return_object_declaration (identifier) @font-lock-constant-face ":" "constant")
     (exception_declaration (identifier) @font-lock-type-face)
     (exception_renaming_declaration :anchor (identifier) @font-lock-type-face)
     (exception_renaming_declaration
      exception_name: (identifier) @font-lock-type-face)
     (exception_renaming_declaration
      exception_name: (selected_component
                       selector_name: (identifier) @font-lock-type-face))
     (raise_expression
      exception_name: (identifier) @font-lock-type-face)
     (raise_expression
      exception_name: (selected_component
                       selector_name: (identifier) @font-lock-type-face))
     (raise_statement name: (identifier) @font-lock-type-face)
     (raise_statement
      name: (selected_component
             selector_name: (identifier) @font-lock-type-face))
     (choice_parameter_specification (identifier) @font-lock-variable-name-face)
     (choice_parameter_specification (identifier) @font-lock-constant-face)
     (parameter_specification (identifier) @font-lock-variable-name-face ":")
     ((parameter_specification
       (identifier) @font-lock-constant-face ":")
      @param-spec
      (:pred ada-ts-mode--mode-in-p @param-spec))
     (formal_object_declaration (identifier) @font-lock-variable-name-face ":")
     ((formal_object_declaration
       (identifier) @font-lock-constant-face ":")
      @object-spec
      (:pred ada-ts-mode--mode-in-p @object-spec))
     (loop_parameter_specification
      :anchor (identifier) @font-lock-variable-name-face)
     (loop_parameter_specification
      :anchor (identifier) @font-lock-constant-face)
     (iterator_specification :anchor (identifier) @font-lock-variable-name-face)
     (discriminant_specification (identifier) @font-lock-variable-name-face ":")
     (discriminant_specification (identifier) @font-lock-constant-face ":")
     (variant_part (identifier) @font-lock-variable-name-face)
     (variant_part (identifier) @font-lock-constant-face)
     (reduction_specification :anchor [(identifier) (string_literal)] @font-lock-function-name-face)
     (reduction_specification :anchor (selected_component
                                       selector_name: _ @font-lock-function-name-face)))

   ;; Function/Procedure Calls
   :language 'ada
   :feature 'function
   :override 'prepend
   '(((function_call
       name: [(identifier) (string_literal)] @font-lock-function-call-face
       :anchor (comment) :*
       :anchor (actual_parameter_part))
      @function-call
      (:pred ada-ts-mode--named-function-call-p @function-call))
     ((function_call
       name: (selected_component
              selector_name: _ @font-lock-function-call-face)
       :anchor (comment) :*
       :anchor (actual_parameter_part))
      @function-call
      (:pred ada-ts-mode--named-function-call-p @function-call))
     (function_call (attribute_designator) @font-lock-function-call-face
                    :anchor (comment) :*
                    :anchor (actual_parameter_part))
     ((procedure_call_statement
       name: (identifier) @font-lock-function-call-face :anchor)
      @procedure-call
      (:pred ada-ts-mode--named-procedure-call-p @procedure-call))
     ((procedure_call_statement
       name: (identifier) @font-lock-function-call-face
       :anchor (comment) :*
       :anchor (actual_parameter_part))
      @procedure-call
      (:pred ada-ts-mode--named-procedure-call-p @procedure-call))
     ((procedure_call_statement
       name: (selected_component
              selector_name: (identifier) @font-lock-function-call-face)
       :anchor)
      @procedure-call
      (:pred ada-ts-mode--named-procedure-call-p @procedure-call))
     ((procedure_call_statement
       name: (selected_component
              selector_name: (identifier) @font-lock-function-call-face)
       :anchor (comment) :*
       :anchor (actual_parameter_part))
      @procedure-call
      (:pred ada-ts-mode--named-procedure-call-p @procedure-call))
     (procedure_call_statement
      (attribute_designator) @font-lock-function-call-face :anchor)
     (procedure_call_statement
      (attribute_designator) @font-lock-function-call-face
      :anchor (comment) :*
      :anchor (actual_parameter_part))
     (reduction_attribute_designator
      (identifier) @font-lock-function-call-face))

   ;; Keywords
   :language 'ada
   :feature 'keyword
   `(([,@ada-ts-mode--keywords] @font-lock-keyword-face)
     ((identifier) @font-lock-keyword-face
      (:match "^[aA][lL][lL]$" @font-lock-keyword-face)))

   ;; Labels
   :language 'ada
   :feature 'label
   '((label statement_identifier: _ @font-lock-constant-face)
     (loop_label statement_identifier: _ @font-lock-constant-face)
     (block_statement
      "end" (identifier) @font-lock-constant-face)
     (loop_statement
      "end" "loop" (identifier) @font-lock-constant-face)
     (exit_statement loop_name: _ @font-lock-constant-face)
     (goto_statement label_name: _ @font-lock-constant-face))

   ;; Numeric literals
   :language 'ada
   :feature 'number
   '((numeric_literal) @font-lock-number-face)

   ;; Operators
   :language 'ada
   :feature 'operator
   :override 'prepend
   `((expression ["and" "else" "or" "then", "xor"] @font-lock-operator-face)
     (factor_power "**" @font-lock-operator-face)
     (factor_abs "abs" @font-lock-operator-face)
     (factor_not "not" @font-lock-operator-face)
     (relation_membership ["not" "in"] @font-lock-operator-face)
     ((relational_operator) @font-lock-operator-face)    ; =, /=, <, >, >=
     ((binary_adding_operator) @font-lock-operator-face) ; +, -, &
     ((unary_adding_operator) @font-lock-operator-face)  ; +, -
     ((multiplying_operator) @font-lock-operator-face)   ; *, /, mod, rem
     ([":=" ".." "|" "=>" "<>" "<<" ">>"] @font-lock-operator-face))

   ;; Preprocessor
   :language 'ada
   :feature 'preprocessor
   :override t
   `(((gnatprep_declarative_if_statement
       [,@ada-ts-mode--preproc-keywords] @font-lock-preprocessor-face))
     ((gnatprep_if_statement
       [,@ada-ts-mode--preproc-keywords] @font-lock-preprocessor-face))
     ((gnatprep_identifier) @font-lock-preprocessor-face))

   ;; String literals
   :language 'ada
   :feature 'string
   '(((string_literal) @font-lock-string-face)
     ((character_literal) @font-lock-constant-face))

   ;; Types
   :language 'ada
   :feature 'type
   '((full_type_declaration (identifier) @font-lock-type-face)
     (record_definition (identifier) @font-lock-type-face) ; Ada 2022
     (incomplete_type_declaration (identifier) @font-lock-type-face)
     (private_type_declaration (identifier) @font-lock-type-face)
     (private_extension_declaration (identifier) @font-lock-type-face)
     (private_extension_declaration (selected_component
                                     selector_name: _ @font-lock-type-face))
     (protected_type_declaration (identifier) @font-lock-type-face)
     (protected_type_declaration (selected_component
                                  selector_name: _ @font-lock-type-face))
     (protected_type_declaration
      (protected_definition "end" (identifier) @font-lock-type-face))
     (single_protected_declaration "new" (identifier) @font-lock-type-face)
     (single_protected_declaration "new" (selected_component
                                          selector_name: _ @font-lock-type-face))
     (task_type_declaration (identifier) @font-lock-type-face)
     (task_type_declaration (selected_component
                             selector_name: _ @font-lock-type-face))
     (task_type_declaration (task_definition endname: _ @font-lock-type-face))
     (single_task_declaration "new" (identifier) @font-lock-type-face)
     (single_task_declaration "new" (selected_component
                                     selector_name: _ @font-lock-type-face))
     (subtype_declaration (identifier) @font-lock-type-face)
     (_ subtype_mark: (selected_component
                       selector_name: _ @font-lock-type-face))
     (_ subtype_mark: (identifier) @font-lock-type-face)
     (_ subtype_mark: (slice prefix: (identifier) @font-lock-type-face))
     (_ subtype_mark: (slice prefix: (selected_component
                                      selector_name: _ @font-lock-type-face)))
     (use_clause "type" (identifier) @font-lock-type-face)
     (use_clause "type" (selected_component
                         selector_name: _ @font-lock-type-face))
     (qualified_expression
      subtype_name: (identifier) @font-lock-type-face)
     (qualified_expression
      subtype_name: (selected_component selector_name: _ @font-lock-type-face))
     (exception_choice
      exception_name: (identifier) @font-lock-type-face)
     (exception_choice
      exception_name: (selected_component
                       selector_name: _ @font-lock-type-face))
     (enumeration_representation_clause local_name: _ @font-lock-type-face)
     (record_representation_clause local_name: _ @font-lock-type-face)
     (record_representation_clause end_local_name: _ @font-lock-type-face) ; Ada 2022
     (formal_complete_type_declaration (identifier) @font-lock-type-face)
     (formal_incomplete_type_declaration (identifier) @font-lock-type-face)
     (formal_derived_type_definition (identifier) @font-lock-type-face)
     (formal_derived_type_definition (selected_component
                                      selector_name: _ @font-lock-type-face))
     (interface_type_definition (identifier) @font-lock-type-face)
     (interface_type_definition (selected_component
                                 selector_name: _ @font-lock-type-face))
     (derived_type_definition (identifier) @font-lock-type-face)
     (derived_type_definition (selected_component
                               selector_name: _ @font-lock-type-face)))

   ;; Syntax errors
   :language 'ada
   :feature 'error
   '((ERROR) @font-lock-warning-face))

  "Font-lock settings for `ada-ts-mode'.")

(defun ada-ts-mode--named-function-call-p (node)
  "Check if NODE is a named function call.

Certain places use a function_call node in the syntax tree, such as a
generic instantiation, because it has similar syntax to a function call,
but it isn't an actual function call."
  (let ((node-type (treesit-node-type node))
        (parent-node-type (treesit-node-type (treesit-node-parent node))))
    (and (string-equal node-type "function_call")
         (not (string-equal parent-node-type "generic_instantiation"))
         (not (string-equal parent-node-type "assignment_statement"))
         (let ((function-name (ada-ts-mode--node-to-name
                               (treesit-node-child-by-field-name node "name")
                               'no-property)))
           (not (string-suffix-p ".all" function-name 'ignore-case))))))

(defun ada-ts-mode--named-procedure-call-p (node)
  "Check if NODE is a named procedure call."
  (let ((node-type (treesit-node-type node)))
    (and (string-equal node-type "procedure_call_statement")
         (let ((procedure-name (ada-ts-mode--node-to-name
                                (treesit-node-child-by-field-name node "name")
                                'no-property)))
           (not (string-suffix-p ".all" procedure-name 'ignore-case))))))

(defun ada-ts-mode--mode-in-p (node)
  "Check if mode for NODE is \\='in\\='."
  (let ((mode-node
         (car
          (treesit-filter-child
           node
           (lambda (n)
             (string-equal
              "non_empty_mode"
              (treesit-node-type n)))))))
    (or (not mode-node) ; implicit mode "in"
        (not (treesit-filter-child
              mode-node
              (lambda (n)
                (string-equal
                 "out"
                 (treesit-node-type n))))))))

;;; Commands

(defun ada-ts-mode-defun-comment-box ()
  "Create comment box for defun enclosing point, if exists."
  (interactive nil ada-ts-mode)
  (when-let* ((defun-node (treesit-defun-at-point))
              (defun-name (ada-ts-mode--defun-name defun-node 'no-property))
              (defun-start (treesit-node-start defun-node))
              (defun-bol
               (save-excursion
                 (goto-char defun-start)
                 (pos-bol)))
              (defun-comment (make-string (length defun-name) ?-))
              (prefix
               (buffer-substring-no-properties defun-bol defun-start)))
    (save-excursion
      (goto-char defun-bol)
      (insert prefix "---" defun-comment "---" ?\n
              prefix "-- " defun-name    " --" ?\n
              prefix "---" defun-comment "---" ?\n ?\n))))

(defun ada-ts-mode-fill-reindent-defun (&optional argument)
  "Refill or re-indent the paragraph or defun containing point.

If the point is in a comment, fill the paragraph that contains point or
follows point.  Otherwise, re-indent the function definition that
contains point.

If ARGUMENT is specified, it is used to specify the column when filling
a paragraph."
  (interactive "P" ada-ts-mode)
  (save-excursion
    (if-let* ((node (treesit-node-at (point)))
              (node-t (treesit-node-type node))
              ((string-equal node-t "comment")))
        (fill-paragraph argument (region-active-p))
      (when-let* ((node (treesit-defun-at-point))
                  (start (treesit-node-start node))
                  (end (treesit-node-end node)))
        (indent-region start end nil)))))

(defun ada-ts-mode-find-other-file (&optional in-other-window)
  "Find other Ada file.

When an LSP client is active, the LSP client dictates where the file is
shown (in the current window or in the other window).

When an LSP client is not active, and if optional IN-OTHER-WINDOW is
non-nil or the \\[universal-argument] prefix is used, find the file in
other window, else find the file in the current window."
  (interactive (list current-prefix-arg) ada-ts-mode)
  (unless (als/other-file)
    (let ((ff-search-directories ff-search-directories))
      (when-let* ((project (project-current))
                  (root (project-root project))
                  (files (project-files project))
                  (dirs (seq-uniq (seq-map #'file-name-directory files))))
        (setq ff-search-directories
              (append
               (list default-directory)
               (ensure-list
                (if (symbolp ff-search-directories)
                    (symbol-value ff-search-directories)
                  ff-search-directories))
               dirs)))
      (ff-find-other-file in-other-window))))

(defun ada-ts-mode--alire-project-file ()
  "Determine name of GNAT Project file, using Alire."
  (let* ((alire-file "alire.toml")
         (alire-path (locate-dominating-file (buffer-file-name) alire-file)))
    (when (and alire-path
               (file-readable-p (expand-file-name alire-file alire-path))
               (executable-find ada-ts-mode-alire-program))
      (let* ((default-directory (file-name-directory alire-path))
             (lines (process-lines ada-ts-mode-alire-program
                                   "--non-interactive" "--no-tty" "show"))
             (file-name
              (seq-first
               (or

                (seq-keep
                 (lambda (line)
                   (when (string-match (rx (+ space)
                                           "Project_File: "
                                           (group (+ anychar)))
                                       line)
                     (match-string 1 line)))
                 lines)
                ;; Use crate name.
                (seq-keep
                 (lambda (line)
                   (when (string-match (rx bos (group (+ (not "="))) "=")
                                       line)
                     (concat (match-string 1 line) ".gpr")))
                 lines)))))
        (expand-file-name file-name)))))

(defun ada-ts-mode--default-project-file ()
  "Determine name of GNAT Project file, looking for default project."
  (when-let* ((gpr-file "default.gpr")
              (gpr-path (locate-dominating-file (buffer-file-name) gpr-file)))
    (expand-file-name gpr-file gpr-path)))

(defun ada-ts-mode--root-project-file ()
  "Determine name of GNAT Project file, looking in root directory."
  (when-let* ((project (project-current))
              (root-dir (project-root project))
              (files (directory-files root-dir nil (rx ".gpr" eos) 'nosort)))
    (when (= (length files) 1)
      (expand-file-name (car files) root-dir))))

(defun ada-ts-mode--project-file ()
  "Determine name of GNAT Project file, if exists."
  (or (als/project-file)
      (ada-ts-mode--alire-project-file)
      (ada-ts-mode--root-project-file)
      (ada-ts-mode--default-project-file)))

(defun ada-ts-mode-find-project-file ()
  "Find GNAT Project file."
  (interactive nil ada-ts-mode)
  (if-let* ((project-file (ada-ts-mode--project-file)))
      (find-file project-file)
    (message "Project file unknown or non-existent.")))

(defvar ada-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-q") #'ada-ts-mode-fill-reindent-defun)
    (when ada-ts-mode-keymap-prefix
      (define-key map
                  (kbd ada-ts-mode-keymap-prefix)
                  (define-keymap
                    "C-b" #'ada-ts-mode-defun-comment-box
                    "C-o" #'ada-ts-mode-find-other-file
                    "C-p" #'ada-ts-mode-find-project-file)))
    map)
  "Keymap for `ada-ts-mode'.")

(easy-menu-define ada-ts-mode-menu ada-ts-mode-map
  "Menu keymap for `ada-ts-mode'."
  '("Ada"
    ["Find Other File"              ada-ts-mode-find-other-file             t]
    ["Find Project File"            ada-ts-mode-find-project-file           t]
    ["-----"                        nil                                     nil]
    ["Toggle Auto-Casing"           ada-ts-auto-case-mode                   t]
    ["Case Format Buffer"           ada-ts-mode-case-format-buffer          t]
    ["Case Format Point/Region"     ada-ts-mode-case-format-dwim            t]
    ["-----"                        nil                                     nil]
    ["Indent Defun / Fill Comment"  ada-ts-mode-fill-reindent-defun         t]
    ["Indent Buffer"                (indent-region (point-min) (point-max)) t]
    ["-----"                        nil                                     nil]
    ["Add Comment Box"              ada-ts-mode-defun-comment-box           t]
    ["-----"                        nil                                     nil]
    ("Language Server"
     ["Find Workspace Configuration File" als/find-workspace-config-file t]
     ["Find User Configuration File"      als/find-user-config-file      t]
     ["Show Composite Configuration"      als/show-composite-config      t])
    ["-----"                        nil                                     nil]
    ["Manual"                       (info "(ada-ts-mode)Top")               t]
    ["Customize"                    (customize-group 'ada-ts)               t]))

;;;###autoload
(define-derived-mode ada-ts-mode prog-mode "Ada"
  "Major mode for editing Ada, powered by tree-sitter."
  :group 'ada-ts

  ;; Grammar.
  (when (and (treesit-available-p)
             (not (treesit-language-available-p 'ada))
             (pcase ada-ts-mode-grammar-install
               ('auto t)
               ('prompt
                ;; Use `read-key' instead of `read-from-minibuffer' as
                ;; this is less intrusive.  The later will start
                ;; `minibuffer-mode' which impacts buffer local
                ;; variables, especially font lock, preventing proper
                ;; mode initialization and results in improper
                ;; fontification of the buffer immediately after
                ;; installing the grammar.
                (let ((y-or-n-p-use-read-key t))
                  (y-or-n-p
                   (format
                    (concat "Tree-sitter grammar for Ada is missing.  "
                            "Install it from %s? ")
                    (car (alist-get 'ada treesit-language-source-alist))))))
               (_ nil)))
    (message "Installing the tree-sitter grammar for Ada")
    (treesit-install-language-grammar 'ada))

  (unless (treesit-ready-p 'ada)
    (error "Tree-sitter for Ada isn't available"))

  (treesit-parser-create 'ada)

  ;; Comments.
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "--" (* "-") (* (syntax whitespace))))

  ;; Syntax.
  (setq-local syntax-propertize-function #'ada-ts-mode--syntax-propertize)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              `(,(rx bos (or "entry_body"
                             "entry_declaration"
                             "expression_function_declaration"
                             "formal_abstract_subprogram_declaration"
                             "formal_concrete_subprogram_declaration"
                             "formal_package_declaration"
                             "generic_instantiation"
                             "generic_package_declaration"
                             "generic_renaming_declaration"
                             "generic_subprogram_declaration"
                             "null_procedure_declaration"
                             "package_body"
                             "package_body_stub"
                             "package_declaration"
                             "package_renaming_declaration"
                             "protected_body"
                             "protected_body_stub"
                             "protected_type_declaration"
                             "single_protected_declaration"
                             "single_task_declaration"
                             "subprogram_body"
                             "subprogram_body_stub"
                             "subprogram_declaration"
                             "subprogram_renaming_declaration"
                             "subunit"
                             "task_body"
                             "task_body_stub"
                             "task_type_declaration")
                     eos)
                .
                ada-ts-mode--defun-p))
  (setq-local treesit-defun-name-function #'ada-ts-mode--defun-name)

  ;; Imenu.
  (setq-local imenu-create-index-function #'ada-ts-imenu)

  ;; Indent.
  (ada-ts-indent--setup)

  ;; Outline minor mode (Emacs 30+)
  (setq-local treesit-outline-predicate #'ada-ts-mode--defun-p)

  ;; EditorConfig (Emacs 30+)
  (setq-local editorconfig-indent-size-vars '(ada-ts-mode-indent-offset))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings ada-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword preprocessor string type)
                (attribute assignment constant function number operator)
                (bracket delimiter error label)))

  ;; Other File.
  (setq-local ff-other-file-alist 'ada-ts-mode-other-file-alist)

  ;; LSP Client.
  (run-hooks 'ada-ts-lspclient-setup-hook)

  (treesit-major-mode-setup)

  ;; Override `treesit-major-mode-setup' settings.
  (setq-local indent-region-function #'ada-ts-mode--indent-region)
  (setq-local indent-line-function   #'ada-ts-mode--indent-line))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               `(,(rx (or ".ada" ".adb" ".ads" ".adc") eos) . ada-ts-mode))
  ;; Add ada-mode as an "extra" parent so ada-ts-mode can handle
  ;; directory local variables for ada-mode, etc. (Emacs 30+)
  (when (fboundp 'derived-mode-add-parents)
    (derived-mode-add-parents 'ada-ts-mode '(ada-mode)))
  ;; Prefer `major-mode-remap-defaults' if available (Emacs 30+)
  (if (boundp 'major-mode-remap-defaults)
      (add-to-list 'major-mode-remap-defaults '(ada-mode . ada-ts-mode))
    (add-to-list 'major-mode-remap-alist '(ada-mode . ada-ts-mode))))

;; Register mode's default grammar
(add-to-list 'treesit-language-source-alist
             `(ada . ,(ensure-list ada-ts-mode-grammar))
             'append)

;; Lazily register mode's info lookup help.
(with-eval-after-load 'info-look
  (declare-function info-lookup-add-help "info-look" (&rest args))
  (info-lookup-add-help
   :topic 'symbol
   :mode '(emacs-lisp-mode . "ada")
   :regexp "\\bada-ts-[^][()`'‘’,\" \t\n]+"
   :doc-spec '(("(ada-ts-mode)Command & Function Index" nil "^ -+ .*: " "\\( \\|$\\)")
               ("(ada-ts-mode)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)"))))

;; Lazily load LSP client support.
(with-eval-after-load 'eglot
  (require 'ada-ts-lspclient-eglot))
(with-eval-after-load 'lsp-mode
  (require 'ada-ts-lspclient-lsp-mode))

(provide 'ada-ts-mode)

;;; ada-ts-mode.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("als/" . "ada-ts-als-"))
;; End:
