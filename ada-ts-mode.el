;;; ada-ts-mode.el --- Major mode for Ada using Tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Troy Brown

;; Author: Troy Brown <brownts@troybrown.dev>
;; Created: February 2023
;; Version: 0.7.4
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

(require 'ada-ts-mode-lspclient)
(require 'lisp-mnt)
(require 'treesit)
(require 'url-parse)
(require 'url-util)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

;;; Customization

(defgroup ada-ts nil
  "Major mode for Ada, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "ada-ts-mode.el")
  :link `(url-link :tag "Website"
                   ,(lm-website (locate-library "ada-ts-mode.el")))
  :link '(custom-manual "(ada-ts-mode)Top")
  :prefix "ada-ts-mode-")

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

(defcustom ada-ts-mode-imenu-categories
  '(package
    subprogram
    protected
    task
    type-declaration
    with-clause)
  "Configuration of Imenu categories."
  :type '(repeat :tag "Categories"
                 (choice :tag "Category"
                         (const :tag "Package" package)
                         (const :tag "Subprogram" subprogram)
                         (const :tag "Protected" protected)
                         (const :tag "Task" task)
                         (const :tag "Type Declaration" type-declaration)
                         (const :tag "With Clause" with-clause)))
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.6.0"))

(defcustom ada-ts-mode-imenu-category-name-alist
  '((package          . "Package")
    (subprogram       . "Subprogram")
    (protected        . "Protected")
    (task             . "Task")
    (type-declaration . "Type Declaration")
    (with-clause      . "With Clause"))
  "Configuration of Imenu category names."
  :type '(alist :key-type symbol :value-type string)
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.6.0"))

(defcustom ada-ts-mode-imenu-nesting-strategy-function
  #'ada-ts-mode-imenu-nesting-strategy-before
  "Configuration for Imenu nesting strategy function."
  :type `(choice (const :tag "Place Before Nested Entries"
                        ,#'ada-ts-mode-imenu-nesting-strategy-before)
                 (const :tag "Place Within Nested Entries"
                        ,#'ada-ts-mode-imenu-nesting-strategy-within)
                 (function :tag "Custom function"))
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.5.8"))

(defcustom ada-ts-mode-imenu-nesting-strategy-placeholder "<<parent>>"
  "Placeholder for an item used in some Imenu nesting strategies."
  :type 'string
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.5.8"))

(defcustom ada-ts-mode-imenu-sort-function #'identity
  "Configuration for Imenu sorting function."
  :type `(choice (const :tag "In Buffer Order" ,#'identity)
                 (const :tag "Alphabetically" ,#'ada-ts-mode-imenu-sort-alphabetically)
                 (function :tag "Custom function"))
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.5.8"))

(defcustom ada-ts-mode-indent-backend 'default
  "Backend used for indentation."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Language Server" lsp))
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.7.0"))
;;;###autoload(put 'ada-ts-mode-indent-backend 'safe-local-variable #'symbolp)

(defcustom ada-ts-mode-indent-offset 3
  "Indentation of statements."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.7.0"))
;;;###autoload(put 'ada-ts-mode-indent-offset 'safe-local-variable #'integerp)

(defcustom ada-ts-mode-other-file-alist
  `((,(rx   ".ads" eos) (  ".adb"))
    (,(rx   ".adb" eos) (  ".ads"))
    (,(rx ".1.ada" eos) (".2.ada"))
    (,(rx ".2.ada" eos) (".1.ada"))
    (,(rx  "_.ada" eos) (  ".ada"))
    (,(rx   ".ada" eos) ( "_.ada")))
  "Ada file extension mapping for \\='find other file\\='."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :group 'ada-ts
  :link '(custom-manual :tag "Navigation" "(ada-ts-mode)Navigation")
  :link '(function-link ff-find-other-file)
  :link '(variable-link ff-other-file-alist)
  :package-version '(ada-ts-mode . "0.7.0"))

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
    "package" "pragma" "private" "procedure" "protected"
    "raise" "range" "record" "renames" "return" "reverse"
    "select" "separate" "some" "subtype" "synchronized"
    "tagged" "task" "terminate" "then" "type"
    "until" "use"
    "when" "while" "with")
  "Ada keywords for tree-sitter font-locking.")

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
     (variant_part (identifier) @font-lock-constant-face))

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

   ;; Control
   :language 'ada
   :feature 'control
   :override 'prepend
   '(["accept" "delay" "entry" "exit" "goto"
      "pragma" "raise" "requeue" "terminate" "until"]
     @font-lock-operator-face)

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
     (incomplete_type_declaration (identifier) @font-lock-type-face)
     (private_type_declaration (identifier) @font-lock-type-face)
     (private_extension_declaration (identifier) @font-lock-type-face)
     (protected_type_declaration (identifier) @font-lock-type-face "is")
     (protected_type_declaration
      (protected_definition "end" (identifier) @font-lock-type-face))
     (task_type_declaration "type"
                            :anchor (comment) :*
                            :anchor (identifier) @font-lock-type-face)
     (task_type_declaration (task_definition endname: _ @font-lock-type-face))
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
     (formal_complete_type_declaration (identifier) @font-lock-type-face)
     (formal_incomplete_type_declaration (identifier) @font-lock-type-face))

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
                               (treesit-node-child-by-field-name node "name"))))
           (not (string-suffix-p ".all" function-name 'ignore-case))))))

(defun ada-ts-mode--named-procedure-call-p (node)
  "Check if NODE is a named procedure call."
  (let ((node-type (treesit-node-type node)))
    (and (string-equal node-type "procedure_call_statement")
         (let ((procedure-name (ada-ts-mode--node-to-name
                                (treesit-node-child-by-field-name node "name"))))
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

;;; Indent Support

(defun ada-ts-mode--lsp-indent-line ()
  "Perform line indentation using LSP server.

Indent relative to previous line for newlines and whenever the region
formatting function fails."
  (if-let* ((client (ada-ts-mode-lspclient-current)))
      (if (string-match (rx bos (zero-or-more space) eos)
                        (buffer-substring (pos-bol) (pos-eol)))
          ;; Handle extraneous space as well as implement a workaround
          ;; for LSP onTypeFormatting for RET as described in
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=70929
          (progn
            ;; Remove whitespace and reindent
            (delete-horizontal-space)
            (indent-relative-first-indent-point))
        (condition-case _
            (ada-ts-mode--lsp-indent-region (pos-bol) (pos-eol) client)
          (error
           ;; The Ada Language Server will return an error if
           ;; attemping to indent when syntax errors exist.  Fallback
           ;; to indenting relative to the previous line when this
           ;; happens.
           (let ((initial-point-column (current-column))
                 (initial-indentation-column (current-indentation)))
             ;; Remove leading whitespace and reindent
             (save-excursion
               (beginning-of-line)
               (delete-horizontal-space)
               (indent-relative-first-indent-point))

             ;; If point was in the whitepace at the begining of the
             ;; line, that whitespace will have been deleted by
             ;; `delete-horizontal-space' and point will end up at the
             ;; beginning of the line.  In this situation, attempt to
             ;; restore point location within new whitespace (created by
             ;; `indent-relative-first-indent-point') when possible,
             ;; otherwise place point at the first non-whitespace
             ;; location.
             (when (<= initial-point-column
                       initial-indentation-column)
               (if (< initial-point-column (current-indentation))
                   (move-to-column initial-point-column)
                 (back-to-indentation)))))))
    ;; fallback on default indentation
    (ada-ts-mode--default-indent-line)))

(defun ada-ts-mode--lsp-indent-region (beg end &optional client)
  "Perform region indentation between BEG and END using LSP server.

When CLIENT is not nil, use it as the active LSP client."
  (if-let* ((client (or client (ada-ts-mode-lspclient-current))))
      (let ((inhibit-message t)
            (tab-width ada-ts-mode-indent-offset)
            (standard-indent ada-ts-mode-indent-offset))
        (ada-ts-mode-lspclient-format-region client beg end))
    ;; fallback on default indentation
    (ada-ts-mode--default-indent-region beg end)))

(defun ada-ts-mode--default-indent-line ()
  "Perform line indentation using default implementation."
  (if-let* ((indent-line (default-value 'indent-line-function)))
      (funcall indent-line)))

(defun ada-ts-mode--default-indent-region (beg end)
  "Perform region indentation between BEG and END using default implementation."
  (if-let* ((indent-region (default-value 'indent-region-function)))
      (funcall indent-region beg end)))

(defun ada-ts-mode--indent-line ()
  "Perform line indentation."
  (let ((indent-line
         (pcase ada-ts-mode-indent-backend
           ('lsp #'ada-ts-mode--lsp-indent-line)
           (_    #'ada-ts-mode--default-indent-line))))
    (funcall indent-line)))

(defun ada-ts-mode--indent-region (beg end)
  "Perform region indentation between BEG and END."
  (let ((indent-region
         (pcase ada-ts-mode-indent-backend
           ('lsp #'ada-ts-mode--lsp-indent-region)
           (_    #'ada-ts-mode--default-indent-region))))
    (funcall indent-region beg end)))

;;; Commands

(defun ada-ts-mode-defun-comment-box ()
  "Create comment box for defun enclosing point, if exists."
  (interactive)
  (when-let* ((defun-node (treesit-defun-at-point))
              (defun-name (treesit-defun-name defun-node))
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

(defun ada-ts-mode-find-other-file ()
  "Find other Ada file."
  (interactive)
  (let ((client (ada-ts-mode-lspclient-current))
        (command "als-other-file"))
    (if (and client
             (ada-ts-mode-lspclient-command-supported-p client command))
        (let ((document-id (ada-ts-mode-lspclient-document-id client)))
          (ada-ts-mode-lspclient-command-execute client command document-id))
      (require 'find-file)
      (ff-find-other-file))))

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

(defun ada-ts-mode--lsp-project-file ()
  "Determine name of GNAT Project file, using Language Server."

  ;; First, check the workspace configuration.  This is preferred if
  ;; the actual project file cannot be found since the Language Server
  ;; will return a default project file name (when queried for the
  ;; project file) when it can't find the configured project file.  In
  ;; this situation we prefer the non-existent user configured project
  ;; file over a non-existent Language Server project file.

  ;; NOTE: Older versions of the Ada Language Server returned a file
  ;; path for the als-project-file command, but newer versions return
  ;; a URI instead.

  (when-let*
      ((client (ada-ts-mode-lspclient-current))
       (project-file-path-or-uri
        (or (ada-ts-mode-lspclient-workspace-configuration client "ada.projectFile")
            (let ((command "als-project-file"))
              (and (ada-ts-mode-lspclient-command-supported-p client command)
                   (ada-ts-mode-lspclient-command-execute client command)))))
       (project-file
        (let* ((obj (url-generic-parse-url (url-unhex-string project-file-path-or-uri)))
               (type (url-type obj)))
          (if (and type (string-equal type "file"))
              (let ((path (url-filename obj)))
                (if (and (eq system-type 'windows-nt)
                         (string-equal (substring path 0 1) "/"))
                    (substring path 1) ; Strip leading separator on Windows
                  path))
            ;; Doesn't appear to be a URI, treat as path
            project-file-path-or-uri)))
       (root (ada-ts-mode-lspclient-workspace-root client (buffer-file-name))))
    ;; The Ada Language Server can return an empty string when it
    ;; can't find the project file.
    (unless (string-empty-p project-file)
      (directory-file-name (expand-file-name project-file root)))))

(defun ada-ts-mode--root-project-file ()
  "Determine name of GNAT Project file, looking in root directory."
  (require 'project)
  (declare-function project-root "project")
  (when-let* ((project (project-current))
              (root-dir (project-root project))
              (files (directory-files root-dir nil (rx ".gpr" eos) 'nosort)))
    (when (= (length files) 1)
      (expand-file-name (car files) root-dir))))

(defun ada-ts-mode--project-file ()
  "Determine name of GNAT Project file, if exists."
  (or (ada-ts-mode--lsp-project-file)
      (ada-ts-mode--alire-project-file)
      (ada-ts-mode--root-project-file)
      (ada-ts-mode--default-project-file)))

(defun ada-ts-mode-find-project-file ()
  "Find GNAT Project file."
  (interactive)
  (if-let* ((project-file (ada-ts-mode--project-file)))
      (find-file project-file)
    (message "Project file unknown or non-existent.")))

;;; Language Server

(defun ada-ts-mode--uri-to-path (uri)
  "Convert URI to file path."
  (let* ((obj (url-generic-parse-url (url-unhex-string uri)))
         (path (url-filename obj)))
    (when (and (eq system-type 'windows-nt)
               (string-equal (substring path 0 1) "/"))
      ;; Strip leading separator on Windows
      (setq path (substring path 1)))
    (directory-file-name path)))

(defun ada-ts-mode--lsp-session-setup ()
  "Perform LSP session setup."
  (when-let* (((derived-mode-p 'ada-ts-mode))
              (client (ada-ts-mode-lspclient-current))
              (command "als-source-dirs")
              ((ada-ts-mode-lspclient-command-supported-p client command))
              (result (ada-ts-mode-lspclient-command-execute client command))
              (source-dirs
               (seq-map
                (lambda (dir-info)
                  (ada-ts-mode--uri-to-path (plist-get dir-info :uri)))
                result)))
    (ada-ts-mode-lspclient-workspace-dirs-add client source-dirs)))

;;; Imenu

(defun ada-ts-mode--node-to-name (node)
  "Return value of NODE as a name string."
  (pcase (treesit-node-type node)
    ((or "identifier" "string_literal")
     (treesit-node-text node t))
    ("selected_component"
     (string-join
      (append (ensure-list (ada-ts-mode--node-to-name
                            (treesit-node-child-by-field-name node "prefix")))
              (list (ada-ts-mode--node-to-name
                     (treesit-node-child-by-field-name node "selector_name"))))
      treesit-add-log-defun-delimiter))))

(defun ada-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
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
      (treesit-node-child-by-field-name node "parent_unit_name")))))

(defun ada-ts-mode--type-declaration-name (node)
  "Return the type declaration name of NODE."
  (ada-ts-mode--node-to-name
   (car (treesit-filter-child
         node
         (lambda (n)
           (string-equal (treesit-node-type n)
                         "identifier"))))))

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

(defun ada-ts-mode-imenu-nesting-strategy-before (item-name marker subtrees)
  "Nesting strategy which places item before the list of nested entries.

An entry is added for ITEM-NAME at item's MARKER location and another
entry is added after it for ITEM-NAME containing SUBTREES."
  (list (cons item-name marker)
        (cons item-name subtrees)))

(defun ada-ts-mode-imenu-nesting-strategy-within (item-name marker subtrees)
  "Nesting strategy which places item within list of nested entries.

An entry is added for ITEM-NAME containing SUBTREES where SUBTREES is
modified to include a `ada-ts-mode-imenu-nesting-strategy-placeholder'
first item at item's MARKER location."
  (let* ((empty-entry
          (cons ada-ts-mode-imenu-nesting-strategy-placeholder marker))
         (new-subtrees
          (cons empty-entry subtrees)))
    (list (cons item-name new-subtrees))))

(defun ada-ts-mode-imenu-sort-alphabetically (items)
  "Alphabetical sort of Imenu ITEMS."
  (sort items
        (lambda (x y)
          (let ((x-name (downcase (car x)))
                (y-name (downcase (car y)))
                (placeholder (downcase
                              ada-ts-mode-imenu-nesting-strategy-placeholder)))
            ;; Always put placeholder first, even if not alphabetical.
            (or (string= x-name placeholder)
                (and (not (string= y-name placeholder))
                     (string< (car x) (car y))))))))

(defun ada-ts-mode--imenu-index (tree item-p branch-p item-name-fn branch-name-fn)
  "Return Imenu index for a specific item category given TREE.

ITEM-P is a predicate for testing the item category's node.
ITEM-NAME-FN determines the name of the item given the item's node.
BRANCH-P is a predicate for determining if a node is a branch.  This is
used to identify higher level nesting structures (i.e., packages,
subprograms, etc.) which encompass the item.  BRANCH-NAME-FN determines
the name of the branch given the branch node."
  (let* ((node (car tree))
         (subtrees
          (funcall ada-ts-mode-imenu-sort-function
                   (mapcan (lambda (tree)
                             (ada-ts-mode--imenu-index tree
                                                       item-p
                                                       branch-p
                                                       item-name-fn
                                                       branch-name-fn))
                           (cdr tree))))
         (marker (set-marker (make-marker)
                             (treesit-node-start node)))
         (item (funcall item-p node))
         (item-name (when item (funcall item-name-fn node)))
         (branch (funcall branch-p node))
         (branch-name (when branch (funcall branch-name-fn node))))
    (cond ((and item (not subtrees))
           (list (cons item-name marker)))
          ((and item subtrees)
           (funcall ada-ts-mode-imenu-nesting-strategy-function
                    item-name marker subtrees))
          ((and branch subtrees)
           (list (cons branch-name subtrees)))
          (t subtrees))))

(defun ada-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root (treesit-buffer-root-node))
         (defun-tree
          (and (seq-intersection '(package subprogram protected task)
                                 ada-ts-mode-imenu-categories)
               (treesit-induce-sparse-tree root #'ada-ts-mode--defun-p)))
         (index-package
          (and (memq 'package ada-ts-mode-imenu-categories)
               (ada-ts-mode--imenu-index defun-tree
                                         #'ada-ts-mode--package-p
                                         #'ada-ts-mode--defun-p
                                         #'ada-ts-mode--defun-name
                                         #'ada-ts-mode--defun-name)))
         (index-subprogram
          (and (memq 'subprogram ada-ts-mode-imenu-categories)
               (ada-ts-mode--imenu-index defun-tree
                                         #'ada-ts-mode--subprogram-p
                                         #'ada-ts-mode--defun-p
                                         #'ada-ts-mode--defun-name
                                         #'ada-ts-mode--defun-name)))
         (index-protected
          (and (memq 'protected ada-ts-mode-imenu-categories)
               (ada-ts-mode--imenu-index defun-tree
                                         #'ada-ts-mode--protected-p
                                         #'ada-ts-mode--defun-p
                                         #'ada-ts-mode--defun-name
                                         #'ada-ts-mode--defun-name)))
         (index-task
          (and (memq 'task ada-ts-mode-imenu-categories)
               (ada-ts-mode--imenu-index defun-tree
                                         #'ada-ts-mode--task-p
                                         #'ada-ts-mode--defun-p
                                         #'ada-ts-mode--defun-name
                                         #'ada-ts-mode--defun-name)))
         (index-type-declaration
          (and (memq 'type-declaration ada-ts-mode-imenu-categories)
               (ada-ts-mode--imenu-index
                (treesit-induce-sparse-tree
                 root
                 (lambda (node)
                   (or (ada-ts-mode--defun-p node)
                       (ada-ts-mode--type-declaration-p node))))
                #'ada-ts-mode--type-declaration-p
                #'ada-ts-mode--defun-p
                #'ada-ts-mode--type-declaration-name
                #'ada-ts-mode--defun-name)))
         (index-with-clause
          (and (memq 'with-clause ada-ts-mode-imenu-categories)
               (ada-ts-mode--imenu-index
                (treesit-induce-sparse-tree
                 root
                 #'ada-ts-mode--with-clause-name-p
                 nil
                 3) ; Limit search depth for speed
                #'identity
                #'ignore
                #'ada-ts-mode--node-to-name
                #'ignore)))
         (imenu-alist
          ;; Respect category ordering in `ada-ts-mode-imenu-categories'
          (mapcar (lambda (category)
                    (let ((name (alist-get category
                                           ada-ts-mode-imenu-category-name-alist))
                          (index (pcase category
                                   ('package          index-package)
                                   ('subprogram       index-subprogram)
                                   ('protected        index-protected)
                                   ('task             index-task)
                                   ('type-declaration index-type-declaration)
                                   ('with-clause      index-with-clause)
                                   (_ (error "Unknown cateogry: %s" category)))))
                      (cons name index)))
                  ada-ts-mode-imenu-categories)))

    ;; Remove empty categories
    (seq-filter (lambda (i) (cdr i)) imenu-alist)))

;;;###autoload
(define-derived-mode ada-ts-mode prog-mode "Ada"
  "Major mode for editing Ada, powered by tree-sitter."
  :group 'ada-ts

  ;; Grammar.
  (setq-local treesit-language-source-alist
              `((ada . ,(ensure-list ada-ts-mode-grammar))))

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
  (setq-local imenu-create-index-function #'ada-ts-mode--imenu)

  ;; Indent.
  (setq-local indent-region-function #'ada-ts-mode--indent-region)
  (setq-local indent-line-function   #'ada-ts-mode--indent-line)
  (setq-local electric-indent-chars (append ";>," electric-indent-chars))

  ;; Outline minor mode (Emacs 30+)
  (setq-local treesit-outline-predicate #'ada-ts-mode--defun-p)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings ada-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword preprocessor string type)
                (attribute assignment constant control function number operator)
                (bracket delimiter error label)))

  ;; Other File.
  (setq-local ff-other-file-alist 'ada-ts-mode-other-file-alist)

  ;; Language Server.
  (add-hook 'ada-ts-mode-lspclient-session-hook #'ada-ts-mode--lsp-session-setup)

  (treesit-major-mode-setup))

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

;; Lazily register mode's info lookup help.
(with-eval-after-load 'info-look
  (declare-function info-lookup-add-help "info-look" (&rest args))
  (info-lookup-add-help
   :topic 'symbol
   :mode '(emacs-lisp-mode . "ada")
   :regexp "\\bada-ts-[^][()`'‘’,\" \t\n]+"
   :doc-spec '(("(ada-ts-mode)Command & Function Index" nil "^ -+ .*: " "\\( \\|$\\)")
               ("(ada-ts-mode)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)"))))

(provide 'ada-ts-mode)

;;; ada-ts-mode.el ends here
