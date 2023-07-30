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

(defcustom ada-ts-mode-indent-when-offset ada-ts-mode-indent-offset
  "Indentation of `when' relative to `case'."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-broken-offset (- ada-ts-mode-indent-offset 1)
  "Indentation for the continuation of a broken line."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-exp-item-offset (- ada-ts-mode-indent-offset 1)
  "Indentation for the continuation of an expression."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-subprogram-is-offset (- ada-ts-mode-indent-offset 1)
  "Indentation of \"is\" in a null procedure or expression function."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

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

(defun ada-ts-mode--indent-recompute (symbol newval operation where)
  "Recompute indentation variables when SYMBOL is changed.

SYMBOL is expected to be `ada-ts-mode-indent-offset', and
OPERATION is queried to check that it is a `set' operation (as
defined by `add-variable-watcher'), otherwise nothing is updated.
Assuming the global value has not been updated by the user, the
indentation variables are updated using the NEWVAL of SYMBOL and
made buffer-local WHERE indicates a buffer-local modification of
SYMBOL, else the default value is updated instead."
  (when (and (eq symbol 'ada-ts-mode-indent-offset)
             (eq operation 'set))
    (dolist (indent-symbol '(ada-ts-mode-indent-when-offset
                             ada-ts-mode-indent-broken-offset
                             ada-ts-mode-indent-subprogram-is-offset
                             ada-ts-mode-indent-exp-item-offset))
      (let* ((valspec (or (custom-variable-theme-value indent-symbol)
                          (get indent-symbol 'standard-value)))
             (cur-custom-value (eval (car valspec)))
             ;; This routine is invoked before SYMBOL is updated to
             ;; NEWVAL so we need to bind it to the new value so the
             ;; other indentation variables are evaluated using the
             ;; updated value.
             (ada-ts-mode-indent-offset newval)
             (new-custom-value (eval (car valspec))))
        ;; Only update if not globally modified by the user outside of
        ;; the customization system (e.g., via `set-default'), or the
        ;; symbol is already buffer local.
        (when (or (eql cur-custom-value (default-value indent-symbol))
                  (and where (buffer-local-boundp indent-symbol where)))
          (if where
              (with-current-buffer where
                (set (make-local-variable indent-symbol) new-custom-value))
            (set-default indent-symbol new-custom-value)))))))

(add-variable-watcher 'ada-ts-mode-indent-offset #'ada-ts-mode--indent-recompute)

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

(defun ada-ts-mode--first-child-matching (parent type)
  "Find first child of PARENT matching TYPE.
Return nil if no child of that type is found."
  (car
   (treesit-filter-child
    parent
    (lambda (n)
      (equal (treesit-node-type n) type)))))

(defun ada-ts-mode--after-first-sibling-p (sibling)
  "Determine if the location of node comes after SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let ((sibling-node
              (ada-ts-mode--first-child-matching parent sibling)))
        (< (treesit-node-start sibling-node) bol))))

(defun ada-ts-mode--before-first-sibling-p (sibling)
  "Determine if the location of node comes before SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let ((sibling-node
              (ada-ts-mode--first-child-matching parent sibling)))
        (> (treesit-node-start sibling-node) bol))))

(defun ada-ts-mode--between-siblings-p (first-sibling last-sibling)
  "Deterine if node is between FIRST-SIBLING and LAST-SIBLING."
  (lambda (node parent bol &rest _)
    (let ((after (ada-ts-mode--after-first-sibling-p first-sibling))
          (before (ada-ts-mode--before-first-sibling-p last-sibling)))
      (and (funcall after node parent bol)
           (funcall before node parent bol)))))



(defun ada-ts-mode--anchor-first-sibling-matching (type &rest types)
  "Position of first sibling of node whose type matches TYPE.

If TYPES is provided, then match the first sibling whose type
matches any of the types in TYPE or TYPES."
  (lambda (_n parent &rest _)
    (let ((all-types (cons type types)))
      (treesit-node-start
       (car
        (treesit-filter-child
         parent
         (lambda (n)
           (member (treesit-node-type n) all-types))))))))

(defun ada-ts-mode--anchor-bol-last-child-of-first-sibling-matching (sibling-type child-type)
  "Position of last child of first sibling matching CHILD-TYPE and SIBLING-TYPE.

If BOL is specified, the position is the start of the indentation
of the line rather than the start position of the node."
  (lambda (_n parent &rest _)
    (let* ((sibling-node
            (car
             (treesit-filter-child
              parent
              (lambda (n)
                (equal (treesit-node-type n) sibling-type)))))
           (child-nodes
            (treesit-filter-child
             sibling-node
             (lambda (n)
               (equal (treesit-node-type n) child-type))))
           (last-child-node (car (reverse child-nodes)))
           (last-child-start (treesit-node-start last-child-node)))
      (save-excursion
        (goto-char last-child-start)
        (back-to-indentation)
        (point)))))


(defun ada-ts-mode--sibling-exists-p (sibling)
  "Determine if SIBLING exists."
  (lambda (_node parent _bol &rest _)
    (ada-ts-mode--first-child-matching parent sibling)))

(defun ada-ts-mode--sibling-child-exists-p (sibling child)
  "Determine if SIBLING and CHILD of SIBLING exists."
  (lambda (_node parent _bol &rest _)
    (when-let ((sibling-node (ada-ts-mode--first-child-matching parent sibling)))
      (ada-ts-mode--first-child-matching sibling-node child))))

(defun ada-ts-mode--next-sibling (node parent bol &rest _)
  "Determine next sibling in PARENT after this NODE or BOL."
  (if node
      (treesit-node-next-sibling node)
    (car
     (treesit-filter-child
      parent
      (lambda (n)
        (> (treesit-node-start n) bol))))))

(defalias 'ada-ts-mode--next-sibling-exists-p
  'ada-ts-mode--next-sibling)

(defun ada-ts-mode--next-sibling-not-matching (type &rest types)
  "Locate next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let ((all-types (cons type types))
          (sibling-node (ada-ts-mode--next-sibling node parent bol)))
      (while (and sibling-node
                  (seq-some (lambda (a-type)
                              (equal (treesit-node-type sibling-node) a-type))
                            all-types))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'ada-ts-mode--next-sibling-not-matching-exists-p
  'ada-ts-mode--next-sibling-not-matching)

(defun ada-ts-mode--anchor-of-next-sibling-not-matching (type &rest types)
  "Determine indentation anchor of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'ada-ts-mode--next-sibling-not-matching all-types) node parent bol)))
      (car (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun ada-ts-mode--offset-of-next-sibling-not-matching (type &rest types)
  "Determine indentation offset of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'ada-ts-mode--next-sibling-not-matching all-types) node parent bol)))
      (cdr (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun ada-ts-mode--anchor-prev-sibling-matching (sibling-type)
  "Locate previous sibling matching SIBLING-TYPE."
  (lambda (_node parent bol &rest _)
    (treesit-node-start
     (car
      (last
       (treesit-filter-child
        parent
        (lambda (n)
          (and
           (equal (treesit-node-type n) sibling-type)
           (< (treesit-node-start n) bol)))))))))

(defun ada-ts-mode--anchor-grand-parent-bol ()
  "Locate BOL of grand-parent."
  (lambda (_node parent _bol &rest _)
    (save-excursion
      (goto-char (treesit-node-start (treesit-node-parent parent)))
      (back-to-indentation)
      (point))))

(defvar ada-ts-mode--indent-rules
  `((ada
     ;; top-level
     ((or (parent-is ,(rx bos "compilation" eos))
          (parent-is ,(rx bos "compilation_unit" eos)))
      column-0 0)
     ;; with_clause / use_clause
     ((and (or (parent-is "with_clause")
               (parent-is "use_clause"))
           (or (node-is "identifier")
               (node-is "selected_component")
               (node-is ","))
           (or (ada-ts-mode--after-first-sibling-p "identifier")
               (ada-ts-mode--after-first-sibling-p "selected_component")))
      (ada-ts-mode--anchor-first-sibling-matching "identifier" "selected_component")
      0)

     ;; subunit
     ((and (parent-is "subunit")
           (or (node-is ,(rx bos "subprogram_body" eos))
               (node-is ,(rx bos "package_body" eos))
               (node-is ,(rx bos "task_body" eos))
               (node-is ,(rx bos "protected_body" eos))))
      column-0 0)
     ((and (parent-is "subunit")
           (or (node-is "identifier")
               (node-is "selected_component")))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)

     ;; aspect_mark_list / aspect_association
     ((node-is "aspect_specification")
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ((node-is "aspect_mark_list")
      parent
      ada-ts-mode-indent-broken-offset)
     ((parent-is "aspect_mark_list")
      parent
      0)
     ((and (parent-is "aspect_association")
           (ada-ts-mode--before-first-sibling-p "=>"))
      parent
      0)

     ;; expression
     ((and (or (parent-is "array_delta_aggregate")
               (parent-is "record_delta_aggregate"))
           (node-is ,(rx bos "expression" eos)))
      parent
      1)
     ((and (parent-is "expression_function_declaration")
           (node-is ,(rx bos "expression" eos)))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)
     ((and (parent-is ,(rx bos "declare_expression"))
           (node-is ,(rx bos "expression" eos)))
      parent
      ada-ts-mode-indent-offset)
     ((and (or (parent-is ,(rx bos "if_expression" eos))
               (parent-is ,(rx bos "elsif_expression_item" eos)))
           (node-is ,(rx bos "expression" eos))
           (ada-ts-mode--after-first-sibling-p "then"))
      parent
      ada-ts-mode-indent-offset)
     ((and (parent-is ,(rx bos "case_expression_alternative" eos))
           (node-is ,(rx bos "expression" eos)))
      parent
      ada-ts-mode-indent-offset)
     ((node-is ,(rx bos "expression" eos))
      parent
      ada-ts-mode-indent-broken-offset)
     ((parent-is ,(rx bos "expression" eos))
      parent
      ada-ts-mode-indent-exp-item-offset)

     ;; discrete_choice_list
     ((parent-is "discrete_choice_list")
      parent
      0)
     ((node-is "discrete_choice_list")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; case_statement / case_statement_alternative
     ((node-is "case_statement_alternative")
      parent
      ada-ts-mode-indent-when-offset)
     ((and (parent-is "case_statement_alternative")
           (ada-ts-mode--after-first-sibling-p "=>"))
      parent
      ada-ts-mode-indent-offset)
     ((and (parent-is ,(rx bos "case_statement" eos))
           no-node
           (ada-ts-mode--between-siblings-p "case_statement_alternative" "end"))
      (ada-ts-mode--anchor-prev-sibling-matching "case_statement_alternative")
      ada-ts-mode-indent-offset)
     ((and (parent-is ,(rx bos "case_statement" eos))
           (or no-node (node-is "comment"))
           (ada-ts-mode--between-siblings-p "is" "end"))
      parent
      ada-ts-mode-indent-when-offset)

     ;; case_expression_alternative
     ((node-is "case_expression_alternative")
      parent
      ada-ts-mode-indent-when-offset)

     ;; if_expression / case_expression / declare_expression / quantified_expression
     ((or (node-is ,(rx bos "if_expression" eos))
          (node-is ,(rx bos "case_expression" eos))
          (node-is ,(rx bos "declare_expression" eos))
          (node-is ,(rx bos "quantified_expression" eos)))
      (ada-ts-mode--anchor-prev-sibling-matching "(")
      1)

     ;; variant_part / variant_list / component_list / record_definition
     ((and (parent-is "record_definition")
           (ada-ts-mode--between-siblings-p "record" "end"))
      parent-bol
      ada-ts-mode-indent-offset)
     ((parent-is "component_list") parent 0)
     ((and (parent-is "variant_part")
           no-node
           (ada-ts-mode--between-siblings-p "variant_list" "end"))
      (ada-ts-mode--anchor-prev-sibling-matching "variant_list")
      ada-ts-mode-indent-offset)
     ((and (parent-is "variant_part")
           (ada-ts-mode--between-siblings-p "is" "end"))
      parent
      ada-ts-mode-indent-when-offset)
     ((parent-is "variant_list") parent 0)
     ((and (parent-is ,(rx bos "variant" eos))
           (ada-ts-mode--after-first-sibling-p "=>"))
      parent
      ada-ts-mode-indent-offset)

     ;; parameter_specification
     ((and (node-is ,(rx bos "parameter_specification" eos))
           (ada-ts-mode--after-first-sibling-p "parameter_specification"))
      (ada-ts-mode--anchor-first-sibling-matching "parameter_specification")
      0)
     ((node-is ,(rx bos "parameter_specification" eos))
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; result_profile
     ((node-is "result_profile")
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ((parent-is "result_profile")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; access_definition
     ((and (field-is "subtype_mark")
           (parent-is "access_definition"))
      parent
      0)

     ;; parameter_association
     ((and (node-is "parameter_association")
           (ada-ts-mode--after-first-sibling-p "parameter_association"))
      (ada-ts-mode--anchor-first-sibling-matching "parameter_association")
      0)
     ((node-is "parameter_association")
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; named_array_aggregate / array_delta_aggregate
     ((and (or (parent-is "named_array_aggregate")
               (parent-is "array_delta_aggregate"))
           (or (node-is "array_component_association")
               (node-is ","))
           (ada-ts-mode--after-first-sibling-p "array_component_association"))
      (ada-ts-mode--anchor-first-sibling-matching "array_component_association")
      0)
     ((and (parent-is "named_array_aggregate")
           (node-is "array_component_association"))
      parent
      1)
     ((and (parent-is "array_delta_aggregate")
           (or (node-is "with")
               (node-is "delta")
               (node-is "array_component_association")))
      (ada-ts-mode--anchor-prev-sibling-matching "expression")
      ada-ts-mode-indent-broken-offset)

     ;; record_component_association_list
     ((parent-is "record_component_association_list")
      parent
      0)

     ;; record_delta_aggregate
     ((and (parent-is "record_delta_aggregate")
           (or (node-is "with")
               (node-is "delta")
               (node-is "record_component_association_list")))
      (ada-ts-mode--anchor-prev-sibling-matching "expression")
      ada-ts-mode-indent-broken-offset)

     ;; enumeration_type_definition
     ((and (parent-is "enumeration_type_definition")
           (or (node-is "identifier")
               (node-is "character_literal")
               (node-is ","))
           (or (ada-ts-mode--after-first-sibling-p "identifier")
               (ada-ts-mode--after-first-sibling-p "character_literal")))
      (ada-ts-mode--anchor-first-sibling-matching "identifier" "character_literal")
      0)
     ((and (parent-is "enumeration_type_definition")
           (or (node-is "identifier")
               (node-is "character_literal")))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)

     ;; pragma_argument_association
     ((and (node-is "pragma_argument_association")
           (ada-ts-mode--after-first-sibling-p "pragma_argument_association"))
      (ada-ts-mode--anchor-first-sibling-matching "pragma_argument_association")
      0)
     ((node-is "pragma_argument_association")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; exception_declaration
     ((parent-is "exception_declaration")
      parent
      0)

     ;; extended_return_object_declaration
     ((node-is "extended_return_object_declaration")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; protected_definition
     ((node-is "protected_definition")
      parent
      ada-ts-mode-indent-offset)
     ((and (parent-is "protected_definition")
           (or (node-is ,(rx bos "private" eos))
               (node-is ,(rx bos "end" eos))))
      grand-parent
      0)
     ((and (parent-is "protected_definition")
           (node-is ,(rx bos "identifier" eos)))
      grand-parent
      ada-ts-mode-indent-broken-offset)
     ((parent-is "protected_definition")
      parent
      0)

     ;; task_definition
     ((node-is "task_definition")
      parent
      ada-ts-mode-indent-offset)
     ((and (parent-is "task_definition")
           (or (node-is ,(rx bos "private" eos))
               (node-is ,(rx bos "end" eos))))
      grand-parent
      0)
     ((and (parent-is "task_definition")
           (node-is ,(rx bos "identifier" eos)))
      grand-parent
      ada-ts-mode-indent-broken-offset)
     ((parent-is "task_definition")
      parent
      0)

     ;; generic_instantiation
     ((and (parent-is "generic_instantiation")
           (field-is "generic_name"))
      parent
      ada-ts-mode-indent-broken-offset)

     ;; discriminant_specification_list / discriminant_specification
     ((and (or (node-is ,(rx bos "discriminant_specification" eos))
               (node-is ";"))
           (ada-ts-mode--after-first-sibling-p "discriminant_specification"))
      (ada-ts-mode--anchor-first-sibling-matching "discriminant_specification")
      0)
     ((node-is "discriminant_specification_list")
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)

     ;; null_procedure_declaration / expression_function_declaration
     ((and (node-is ,(rx bos "is" eos))
           (or (parent-is "expression_function_declaration")
               (parent-is "null_procedure_declaration")))
      parent-bol
      ada-ts-mode-indent-subprogram-is-offset)
     ((and (node-is ,(rx bos "null" eos))
           (parent-is "null_procedure_declaration"))
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ;; keywords / semicolon
     ((and (node-is ,(rx bos "exception" eos))
           (parent-is "handled_sequence_of_statements"))
      (ada-ts-mode--anchor-grand-parent-bol)
      0)
     ;; prevent keywords from aligning to parent BOL.
     ((and (or (node-is ,(rx bos "then" eos))
               (node-is ,(rx bos "else" eos))
               (node-is "elsif_expression_item"))
           (parent-is ,(rx bos "if_expression" eos)))
      parent
      0)
     ;; prevent keywords from aligning to parent BOL.
     ((and (node-is ,(rx bos "is" eos))
           (parent-is ,(rx bos "case_expression" eos)))
      parent
      0)
     ;; prevent keywords from aligning to parent BOL.
     ((and (parent-is ,(rx bos "declare_expression" eos))
           (node-is ,(rx bos "begin" eos)))
      parent
      0)
     ((and (parent-is ,(rx bos "declare_expression" eos))
           (ada-ts-mode--before-first-sibling-p "begin"))
      parent
      ada-ts-mode-indent-offset)
     ((node-is ,(rx bos "quantifier" eos))
      parent
      ada-ts-mode-indent-broken-offset)
     ((or (node-is ,(eval `(rx bos (or ,@ada-ts-mode--keywords ";") eos)))
          (node-is "record_type_definition")
          (node-is "record_definition")
          (node-is "elsif_statement_item")
          (node-is "aspect_specification")
          (node-is "null_exclusion")
          (node-is "access_to_object_definition")
          (node-is "access_to_subprogram_definition")
          (node-is "procedure_specification")
          (node-is "function_specification")
          (node-is ,(rx bos "allocator" eos)))
      parent-bol
      0)

     ;; loop_statement / loop_parameter_specification / iterator_specification
     ((and (parent-is "loop_statement")
           (or (node-is "loop_label")
               (node-is "iteration_scheme")))
      parent
      0)
     ((node-is "loop_parameter_specification")
      parent
      ada-ts-mode-indent-broken-offset)
     ((parent-is "loop_parameter_specification")
      parent
      0)
     ((node-is "iterator_specification")
      parent
      ada-ts-mode-indent-broken-offset)
     ((parent-is "iterator_specification")
      parent
      0)

     ;; handled_sequence_of_statements / exception_handler / exception_choice_list
     ((and (parent-is "exception_choice_list")
           (ada-ts-mode--after-first-sibling-p "exception_choice"))
      (ada-ts-mode--anchor-first-sibling-matching "exception_choice")
      0)
     ((and (parent-is "exception_handler")
           (ada-ts-mode--after-first-sibling-p "=>"))
      parent
      ada-ts-mode-indent-offset)
     ((parent-is "exception_handler")
      parent
      ada-ts-mode-indent-broken-offset)
     ((and (node-is "exception_handler")
           (ada-ts-mode--after-first-sibling-p "exception_handler"))
      (ada-ts-mode--anchor-first-sibling-matching "exception_handler")
      0)
     ((and (parent-is "handled_sequence_of_statements")
           no-node
           (ada-ts-mode--after-first-sibling-p "exception_handler"))
      (ada-ts-mode--anchor-prev-sibling-matching "exception_handler")
      ada-ts-mode-indent-offset)
     ((and (ada-ts-mode--between-siblings-p
            "handled_sequence_of_statements"
            "end")
           (ada-ts-mode--sibling-child-exists-p
            "handled_sequence_of_statements"
            "exception_handler")
           no-node)
      (ada-ts-mode--anchor-bol-last-child-of-first-sibling-matching
       "handled_sequence_of_statements"
       "exception_handler")
      ada-ts-mode-indent-offset)
     ((parent-is "handled_sequence_of_statements")
      (ada-ts-mode--anchor-grand-parent-bol)
      ada-ts-mode-indent-offset)


     (
      (or (and (or (parent-is "subprogram_body")
                   (parent-is "package_body")
                   (parent-is "package_declaration")
                   (parent-is "task_body")
                   (parent-is "entry_body")
                   (parent-is "protected_body"))
               (or (ada-ts-mode--between-siblings-p "is" "end")
                   ;; (ada-ts-mode--between-siblings-p "begin" "end")
                   ))
          (and (parent-is "extended_return_statement")
               (ada-ts-mode--between-siblings-p "do" "end"))
          (and (parent-is "block_statement")
               (or (ada-ts-mode--between-siblings-p "declare" "begin")
                   (ada-ts-mode--between-siblings-p "begin" "end")
                   ))
          (and (parent-is "loop_statement")
               (ada-ts-mode--between-siblings-p "loop" "end"))
          (and (parent-is ,(rx bos "if_statement" eos))
               (ada-ts-mode--between-siblings-p "then" "end"))
          (and (parent-is "elsif_statement_item")
               (ada-ts-mode--after-first-sibling-p "then"))
          )
      parent-bol
      ada-ts-mode-indent-offset)

     ;; non_empty_declarative_part
     ;; ((node-is "non_empty_declarative_part") ; first item / pragma
     ;;  parent
     ;;  ada-ts-mode-indent-offset)
     ((parent-is "non_empty_declarative_part") ; remaining items / pragmas
      grand-parent
      ada-ts-mode-indent-offset)


     ;; general indentation for newline and comments.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and (or no-node (node-is "comment"))
           (ada-ts-mode--next-sibling-not-matching-exists-p "comment" "ERROR"))
      (ada-ts-mode--anchor-of-next-sibling-not-matching "comment" "ERROR")
      (ada-ts-mode--offset-of-next-sibling-not-matching "comment" "ERROR"))

     ;; identifier / selected_component
     ((or (node-is "identifier")
          (node-is "selected_component")
          (parent-is "selected_component"))
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; non-expression opening parenthesis
     ;; ((and (node-is "(")
     ;;       (parent-is "pragma_g"))
     ;;  (ada-ts-mode--anchor-first-sibling-matching "identifier")
     ;;  ada-ts-mode-indent-broken-offst)
     ((or (node-is "formal_part")
          (node-is "enumeration_aggregate")
          (node-is "enumeration_type_definition")
          (node-is "actual_parameter_part")
          (node-is "known_discriminant_part")
          (node-is "unknown_discriminant_part")
          (node-is "("))
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ;; closing parenthesis (including expression)
     ((and (node-is ")")
           (ada-ts-mode--sibling-exists-p "("))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      0)
     ((and (node-is "]")
           (ada-ts-mode--sibling-exists-p "["))
      (ada-ts-mode--anchor-first-sibling-matching "[")
      0)

     ((or (node-is ,(rx bos ":" eos))
          (node-is ,(rx bos ":=" eos)))
      parent 0)

     ((node-is "=>")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; trival recovery for syntax error or unexpected broken line
     ;; (catch-all prev-line 0)

     ))
  "Tree-sitter indent rules for `ada-ts-mode'.")


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
  (setq-local treesit-simple-indent-rules ada-ts-mode--indent-rules)
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
