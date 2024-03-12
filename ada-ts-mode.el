;;; ada-ts-mode.el --- Major mode for Ada using Tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Troy Brown

;; Author: Troy Brown <brownts@troybrown.dev>
;; Created: February 2023
;; Version: 0.5.7
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

(require 'info-look)
(require 'lisp-mnt)
(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup ada-ts nil
  "Major mode for Ada, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "ada-ts-mode.el")
  :link `(url-link :tag "Website"
                   ,(lm-website (locate-library "ada-ts-mode.el")))
  :link '(custom-manual "(ada-ts-mode)Top")
  :prefix "ada-ts-mode-")

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
  :package-version "0.5.0")

(defcustom ada-ts-mode-grammar-install 'prompt
  "Configuration for installation of tree-sitter language grammar library."
  :type '(choice (const :tag "Automatically Install" auto)
                 (const :tag "Prompt to Install" prompt)
                 (const :tag "Do not install" nil))
  :group 'ada-ts
  :link '(custom-manual :tag "Grammar Installation" "(ada-ts-mode)Grammar Installation")
  :package-version "0.5.0")

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

This is necessary to suppress interpreting syntactic meaning from
a chararacter literal (e.g., double-quote character incorrectly
interpreted as the beginning or end of a string).  The
single-quote character is not defined in the syntax table as a
string since it is also used with attributes.  Thus, it is
defined in the syntax table as punctuation and we identify
character literal instances here and apply the string property to
those instances."
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
     ((range_attribute_designator) @font-lock-property-use-face)
     (component_declaration (identifier) @font-lock-property-name-face)
     (component_choice_list (identifier) @font-lock-property-name-face)
     (component_clause local_name: _ @font-lock-property-name-face))

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
            (term name: (identifier) @font-lock-constant-face)))))))))

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
     (single_task_declaration "task"
                              :anchor (comment) :*
                              :anchor (identifier) @font-lock-variable-name-face)
     (single_task_declaration
      (task_definition "end" (identifier) @font-lock-variable-name-face))
     (task_body (identifier) @font-lock-variable-name-face)
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
      generic_name: (selected_component
                     selector_name: _ @font-lock-function-name-face))
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
   '((function_call
      name: [(identifier) (string_literal)] @font-lock-function-call-face)
     (function_call
      name: (selected_component
             selector_name: _ @font-lock-function-call-face))
     (procedure_call_statement
      name: (identifier) @font-lock-function-call-face)
     (procedure_call_statement
      name: (selected_component
             selector_name: (identifier) @font-lock-function-call-face)))

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
     (allocator "new"
                :anchor (comment) :*
                :anchor (identifier) @font-lock-type-face)
     (allocator "new"
                :anchor (comment) :*
                :anchor (selected_component
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
    (_ t)))

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
    (_ t)))

(defun ada-ts-mode--defun-p (node)
  "Determine if NODE is candidate for defun."
  (pcase (treesit-node-type node)
    ("package_declaration"
     (not (string-equal "generic_package_declaration"
                        (treesit-node-type (treesit-node-parent node)))))
    (_ t)))

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
  (setq-local treesit-simple-imenu-settings
              `(("Package"
                 ,(rx bos (or "formal_package_declaration"
                              "generic_instantiation"
                              "generic_package_declaration"
                              "generic_renaming_declaration"
                              "package_body"
                              "package_body_stub"
                              "package_declaration"
                              "package_renaming_declaration")
                      eos)
                 ada-ts-mode--package-p
                 nil)
                ("Subprogram"
                 ,(rx bos (or "expression_function_declaration"
                              "formal_abstract_subprogram_declaration"
                              "formal_concrete_subprogram_declaration"
                              "generic_instantiation"
                              "generic_renaming_declaration"
                              "generic_subprogram_declaration"
                              "null_procedure_declaration"
                              "subprogram_body"
                              "subprogram_body_stub"
                              "subprogram_declaration"
                              "subprogram_renaming_declaration")
                      eos)
                 ada-ts-mode--subprogram-p
                 nil)))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings ada-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword preprocessor string type)
                (attribute assignment constant control function number operator)
                (bracket delimiter error label)))

  (treesit-major-mode-setup))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               `(,(rx (or ".adb" ".ads" ".adc") eos) . ada-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ada-mode . ada-ts-mode)))

(info-lookup-add-help
 :topic 'symbol
 :mode '(emacs-lisp-mode . "ada")
 :regexp "\\bada-ts-[^][()`'‘’,\" \t\n]+"
 :doc-spec '(("(ada-ts-mode)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)")))

(provide 'ada-ts-mode)

;;; ada-ts-mode.el ends here
