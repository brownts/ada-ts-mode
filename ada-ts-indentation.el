;;; ada-ts-indentation.el -- Indentation support in Ada files -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Troy Brown

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

(require 'ada-ts-als)
(require 'ada-ts-common)
(require 'cl-generic)
(require 'treesit)
(eval-when-compile (require 'rx))

(ada-ts-mode--declare-treesit-functions)

;;;; Customization

(defcustom ada-ts-mode-indent-backend 'tree-sitter
  "Backend used for indentation."
  :type '(choice (const :tag "Tree-sitter" tree-sitter)
                 (const :tag "Language Server" lsp))
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))
;;;###autoload(put 'ada-ts-mode-indent-backend 'safe-local-variable #'symbolp)

(defcustom ada-ts-mode-indent-strategy 'aggressive
  "Indentation strategy utilized with tree-sitter backend."
  :type '(choice :tag "Indentation Strategy"
                 (const :tag "Aggressive" aggressive)
                 (const :tag "Line" line))
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-offset 3
  "Indentation of statements."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.7.0"))
;;;###autoload(put 'ada-ts-mode-indent-offset 'safe-local-variable #'integerp)

(defcustom ada-ts-mode-indent-when-offset ada-ts-mode-indent-offset
  "Indentation of \\='when\\=' relative to \\='case\\='."
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

(defcustom ada-ts-mode-indent-exp-item-offset 0
  "Indentation for the continuation of an expression."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-subprogram-is-offset (- ada-ts-mode-indent-offset 1)
  "Indentation of \\='is\\=' in a null procedure or expression function."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-record-offset ada-ts-mode-indent-offset
  "Indentation of record definition in a type or representation clause."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-label-offset ada-ts-mode-indent-offset
  "Indentation for block and loop statements containing a label."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

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
                             ada-ts-mode-indent-exp-item-offset
                             ada-ts-mode-indent-subprogram-is-offset
                             ada-ts-mode-indent-record-offset
                             ada-ts-mode-indent-label-offset))
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

;;; Formal Indentation Rules

(defvar ada-ts-mode--indent-rules nil
  "Tree-sitter indent rules for `ada-ts-mode'.")

(with-eval-after-load 'ada-ts-mode

  (setq ada-ts-mode--indent-rules
        `((ada

           ((or (matcher/node-is "ERROR")
                (matcher/parent-is "ERROR")
                (matcher/after-first-sibling-p "ERROR")
                no-node) ; newline
            (anchor/best-effort)
            (offset/best-effort))

           ;; top-level
           ((or (matcher/parent-is "compilation")
                (matcher/parent-is "compilation_unit"))
            ;; (query ([(compilation      _ @node)
            ;;          (compilation_unit _ @node)]))
            column-0 0)

           ;; with_clause / use_clause
           ((and (or (matcher/parent-is "with_clause")
                     (matcher/parent-is "use_clause"))
                 (or (matcher/node-is "identifier")
                     (matcher/node-is "selected_component")
                     (matcher/node-is ","))
                 (or (matcher/after-first-sibling-p "identifier")
                     (matcher/after-first-sibling-p "selected_component")))
            (anchor/first-sibling "identifier" "selected_component")
            0)

           ;; subunit
           ((and (matcher/parent-is "subunit")
                 (or (matcher/node-is "subprogram_body")
                     (matcher/node-is "package_body")
                     (matcher/node-is "task_body")
                     (matcher/node-is "protected_body")))
            column-0 0)
           ((and (matcher/parent-is "subunit")
                 (or (matcher/node-is "identifier")
                     (matcher/node-is "selected_component")))
            (anchor/first-sibling "(") 1)

           ;; aspect_mark_list / aspect_association
           ((matcher/node-is "aspect_specification") parent-bol ada-ts-mode-indent-broken-offset)
           ((matcher/node-is "aspect_mark_list")     parent     ada-ts-mode-indent-broken-offset)
           ((matcher/parent-is "aspect_mark_list")   parent     0)
           ((and (matcher/parent-is "aspect_association")
                 (matcher/before-first-sibling-p "=>"))
            parent 0)

           ;; expression
           ((and (matcher/node-is "expression")
                 (or (matcher/parent-is "array_delta_aggregate")
                     (matcher/parent-is "record_delta_aggregate")))
            parent 1)
           ((and (matcher/node-is "expression")
                 (matcher/parent-is "expression_function_declaration"))
            (anchor/first-sibling "(")
            1)
           ((and (matcher/node-is "expression")
                 (or (matcher/parent-is "case_expression_alternative")
                     (matcher/parent-is "declare_expression")
                     (and (matcher/parent-is "if_expression")
                          (matcher/after-first-sibling-p "then"))
                     (and (matcher/parent-is "elsif_expression_item")
                          (matcher/after-first-sibling-p "then"))))
            parent
            ada-ts-mode-indent-offset)

           ((matcher/node-is "expression")   parent ada-ts-mode-indent-broken-offset)
           ((matcher/parent-is "expression") parent ada-ts-mode-indent-exp-item-offset)

           ;; discrete_choice_list
           ((matcher/parent-is "discrete_choice_list") parent 0)
           ((matcher/node-is "discrete_choice_list")   parent ada-ts-mode-indent-broken-offset)

           ;; case_statement / case_statement_alternative
           ((matcher/node-is "case_statement_alternative") parent ada-ts-mode-indent-when-offset)
           ((and (matcher/parent-is "case_statement_alternative")
                 (matcher/after-first-sibling-p "=>"))
            parent
            ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "case_statement")
                 (matcher/node-is "comment")
                 (matcher/between-siblings-p "is" "end"))
            parent
            ada-ts-mode-indent-when-offset)

           ;; case_expression_alternative
           ((matcher/node-is "case_expression_alternative") parent ada-ts-mode-indent-when-offset)

           ;; if_expression / case_expression / declare_expression / quantified_expression
           ((or (matcher/node-is "if_expression")
                (matcher/node-is "case_expression")
                (matcher/node-is "declare_expression")
                (matcher/node-is "quantified_expression"))
            (anchor/prev-sibling "(")
            1)

           ;; variant_part / variant_list / component_list / record_definition
           ((matcher/parent-is "component_list") parent 0)
           ((and (matcher/parent-is "variant_part")
                 (matcher/between-siblings-p "is" "end"))
            parent
            ada-ts-mode-indent-when-offset)
           ((matcher/parent-is "variant_list") parent 0)
           ((and (matcher/parent-is "variant")
                 (matcher/after-first-sibling-p "=>"))
            parent
            ada-ts-mode-indent-offset)

           ;; parameter_specification
           ((and (matcher/node-is "parameter_specification")
                 (matcher/after-first-sibling-p "parameter_specification"))
            (anchor/first-sibling "parameter_specification")
            0)
           ((matcher/node-is "parameter_specification") parent-bol ada-ts-mode-indent-broken-offset)

           ;; result_profile
           ((matcher/node-is   "result_profile") parent-bol ada-ts-mode-indent-broken-offset)
           ((matcher/parent-is "result_profile") parent     ada-ts-mode-indent-broken-offset)

           ;; access_definition
           ((and (matcher/parent-is "access_definition")
                 (matcher/field-is "subtype_mark"))
            parent
            0)

           ;; parameter_association
           ((and (matcher/node-is "parameter_association")
                 (matcher/after-first-sibling-p "parameter_association"))
            (anchor/first-sibling "parameter_association")
            0)
           ((matcher/node-is "parameter_association") parent-bol ada-ts-mode-indent-broken-offset)

           ;; named_array_aggregate / array_delta_aggregate
           ((and (or (matcher/parent-is "named_array_aggregate")
                     (matcher/parent-is "array_delta_aggregate"))
                 (or (matcher/node-is "array_component_association")
                     (matcher/node-is ","))
                 (matcher/after-first-sibling-p "array_component_association"))
            (anchor/first-sibling "array_component_association")
            0)
           ((and (matcher/parent-is "named_array_aggregate")
                 (matcher/node-is "array_component_association"))
            parent
            1)
           ((and (matcher/parent-is "array_delta_aggregate")
                 (or (matcher/node-is "with")
                     (matcher/node-is "delta")
                     (matcher/node-is "array_component_association")))
            (anchor/prev-sibling "expression")
            ada-ts-mode-indent-broken-offset)

           ;; record_component_association_list
           ((matcher/parent-is "record_component_association_list") parent 0)

           ;; record_delta_aggregate
           ((and (matcher/parent-is "record_delta_aggregate")
                 (or (matcher/node-is "with")
                     (matcher/node-is "delta")
                     (matcher/node-is "record_component_association_list")))
            (anchor/prev-sibling "expression")
            ada-ts-mode-indent-broken-offset)

           ;; enumeration_type_definition
           ((and (matcher/parent-is "enumeration_type_definition")
                 (or (matcher/node-is "identifier")
                     (matcher/node-is "character_literal")
                     (matcher/node-is ","))
                 (or (matcher/after-first-sibling-p "identifier")
                     (matcher/after-first-sibling-p "character_literal")))
            (anchor/first-sibling "identifier" "character_literal")
            0)
           ((and (matcher/parent-is "enumeration_type_definition")
                 (or (matcher/node-is "identifier")
                     (matcher/node-is "character_literal")))
            (anchor/first-sibling "(")
            1)

           ;; pragma_argument_association
           ((and (matcher/node-is "pragma_argument_association")
                 (matcher/after-first-sibling-p "pragma_argument_association"))
            (anchor/first-sibling "pragma_argument_association")
            0)
           ((matcher/node-is "pragma_argument_association") parent ada-ts-mode-indent-broken-offset)

           ;; exception_declaration
           ((matcher/parent-is "exception_declaration") parent 0)

           ;; extended_return_object_declaration
           ((matcher/node-is "extended_return_object_declaration") parent ada-ts-mode-indent-broken-offset)

           ;; protected_definition
           ((and (matcher/parent-is "protected_definition")
                 (or (matcher/node-is "private")
                     (matcher/node-is "end"))
                 (n-p-gp nil nil "ERROR"))
            (anchor/best-effort)
            (offset/best-effort))
           ((and (matcher/parent-is "protected_definition")
                 (matcher/node-is "identifier")
                 (matcher/after-first-sibling-p "end")
                 (n-p-gp nil nil "ERROR"))
            (anchor/best-effort)
            (offset/best-effort))
           ((and (matcher/node-is "protected_definition")
                 (or (matcher/first-child-is "private")
                     (matcher/first-child-is "end")))
            parent
            0)
           ((matcher/node-is "protected_definition") parent ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "protected_definition")
                 (or (matcher/node-is "private")
                     (matcher/node-is "end")))
            grand-parent
            0)
           ((and (matcher/parent-is "protected_definition")
                 (matcher/node-is "identifier")
                 (matcher/after-first-sibling-p "end"))
            grand-parent
            ada-ts-mode-indent-broken-offset)
           ((and (matcher/parent-is "protected_definition")
                 (matcher/first-sibling-is "private")
                 (matcher/after-first-sibling-p "private"))
            parent
            ada-ts-mode-indent-offset)
           ((matcher/parent-is "protected_definition") parent 0)

           ;; task_definition
           ((and (matcher/parent-is "task_definition")
                 (or (matcher/node-is "private")
                     (matcher/node-is "end"))
                 (n-p-gp nil nil "ERROR"))
            (anchor/best-effort)
            (offset/best-effort))
           ((and (matcher/parent-is "task_definition")
                 (matcher/node-is "identifier")
                 (matcher/after-first-sibling-p "end")
                 (n-p-gp nil nil "ERROR"))
            (anchor/best-effort)
            (offset/best-effort))
           ((and (matcher/node-is "task_definition")
                 (or (matcher/first-child-is "private")
                     (matcher/first-child-is "end")))
            parent
            0)
           ((matcher/node-is "task_definition") parent ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "task_definition")
                 (or (matcher/node-is "private")
                     (matcher/node-is "end")))
            grand-parent
            0)
           ((and (matcher/parent-is "task_definition")
                 (matcher/node-is "identifier")
                 (matcher/after-first-sibling-p "end"))
            grand-parent
            ada-ts-mode-indent-broken-offset)
           ((and (matcher/parent-is "task_definition")
                 (matcher/first-sibling-is "private")
                 (matcher/after-first-sibling-p "private"))
            parent
            ada-ts-mode-indent-offset)
           ((matcher/parent-is "task_definition") parent 0)

           ;; generic_instantiation
           ((and (matcher/parent-is "generic_instantiation")
                 (matcher/field-is "generic_name"))
            parent
            ada-ts-mode-indent-broken-offset)

           ;; discriminant_specification_list / discriminant_specification
           ((and (or (matcher/node-is "discriminant_specification")
                     (matcher/node-is ";"))
                 (matcher/after-first-sibling-p "discriminant_specification"))
            (anchor/first-sibling "discriminant_specification")
            0)
           ((matcher/node-is "discriminant_specification_list")
            (anchor/first-sibling "(")
            1)

           ;; null_procedure_declaration / expression_function_declaration / abstract subprogram_declaration
           ((or (and (or (matcher/parent-is "expression_function_declaration")
                         (matcher/parent-is "null_procedure_declaration"))
                     (matcher/node-is "is"))
                (and (matcher/parent-is "subprogram_declaration")
                     (matcher/node-is "is")
                     (matcher/before-first-sibling-p "abstract")))
            parent-bol
            ada-ts-mode-indent-subprogram-is-offset)
           ((or (and (matcher/parent-is "null_procedure_declaration")
                     (matcher/node-is "null"))
                (and (matcher/parent-is "subprogram_declaration")
                     (matcher/node-is "abstract")))
            parent-bol
            ada-ts-mode-indent-broken-offset)

           ;; handled_sequence_of_statements
           ((and (matcher/parent-is "handled_sequence_of_statements")
                 (matcher/node-is "exception")
                 (n-p-gp nil nil "ERROR"))
            (anchor/best-effort)
            (offset/best-effort))
           ((and (matcher/parent-is "handled_sequence_of_statements")
                 (matcher/node-is "exception"))
            (anchor/gp-skip-label-bol)
            0)
           ((and (matcher/parent-is "handled_sequence_of_statements")
                 (matcher/after-first-sibling-p "exception_handler"))
            (anchor/first-sibling "exception_handler")
            0)
           ((matcher/parent-is "handled_sequence_of_statements") parent-bol 0)
           ((and (matcher/parent-is "exception_handler")
                 (matcher/after-first-sibling-p "=>"))
            parent
            ada-ts-mode-indent-offset)
           ((matcher/parent-is "exception_handler") parent ada-ts-mode-indent-broken-offset)
           ((and (matcher/parent-is "exception_choice_list")
                 (matcher/after-first-sibling-p "exception_choice"))
            (anchor/first-sibling "exception_choice")
            0)

           ;; non_empty_declarative_part
           ((matcher/parent-is "non_empty_declarative_part") parent-bol 0)

           ;; prevent keywords from aligning to parent BOL.
           ((or (and (matcher/parent-is "if_expression")
                     (or (matcher/node-is "then")
                         (matcher/node-is "else")
                         (matcher/node-is "elsif_expression_item")))
                (and (matcher/parent-is "case_expression")
                     (matcher/node-is "is"))
                (and (matcher/parent-is "declare_expression")
                     (matcher/node-is "begin")))
            parent
            0)
           ((and (matcher/parent-is "declare_expression")
                 (matcher/before-first-sibling-p "begin"))
            parent
            ada-ts-mode-indent-offset)
           ((matcher/node-is "quantifier") parent ada-ts-mode-indent-broken-offset)

           ;; Handle special record type indentation.
           ((or (matcher/node-is "record_definition")
                (matcher/node-is "record_type_definition")
                (matcher/node-is "record_representation_clause"))
            (anchor/best-effort)
            (offset/best-effort))
           ((or (matcher/parent-is "record_definition")
                (matcher/parent-is "record_type_definition")
                (matcher/parent-is "record_representation_clause"))
            (anchor/best-effort)
            (offset/best-effort))

           ;; loop_statement
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "loop_label")
                 (matcher/node-is "iteration_scheme"))
            parent
            ada-ts-mode-indent-label-offset)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "iteration_scheme")
                 (matcher/between-siblings-p "loop" "end"))
            (anchor/first-sibling-bol "iteration_scheme")
            ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "iteration_scheme")
                 (or (matcher/node-is "loop")
                     (matcher/node-is "end")
                     (matcher/node-is ";")))
            (anchor/first-sibling-bol "iteration_scheme")
            0)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "iteration_scheme")
                 (matcher/node-is "identifier"))
            (anchor/first-sibling-bol "iteration_scheme")
            ada-ts-mode-indent-broken-offset)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "loop_label")
                 (matcher/node-is "loop"))
            parent
            ada-ts-mode-indent-label-offset)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/between-siblings-p "loop" "end"))
            (anchor/first-sibling-bol "loop")
            ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "loop")
                 (or (matcher/node-is "loop")
                     (matcher/node-is "end")
                     (matcher/node-is ";")))
            (anchor/first-sibling-bol "loop")
            0)
           ((and (matcher/parent-is "loop_statement")
                 (matcher/after-first-sibling-p "loop")
                 (matcher/node-is "identifier"))
            (anchor/first-sibling-bol "loop")
            ada-ts-mode-indent-broken-offset)
           ((matcher/node-is   "loop_parameter_specification") parent ada-ts-mode-indent-broken-offset)
           ((matcher/parent-is "loop_parameter_specification") parent 0)
           ((matcher/node-is   "iterator_specification") parent ada-ts-mode-indent-broken-offset)
           ((matcher/parent-is "iterator_specification") parent 0)

           ;; block_statement
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "loop_label")
                 (matcher/node-is "declare"))
            parent
            ada-ts-mode-indent-label-offset)
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "declare")
                 (or (matcher/node-is "begin")
                     (matcher/node-is "end")
                     (matcher/node-is ";")))
            (anchor/first-sibling-bol "declare")
            0)
           ((and (matcher/parent-is "block_statement")
                 (matcher/between-siblings-p "declare" "begin"))
            (anchor/first-sibling-bol "declare")
            ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "declare")
                 (matcher/between-siblings-p "begin" "end"))
            (anchor/first-sibling-bol "declare")
            ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "declare")
                 (matcher/node-is "identifier"))
            (anchor/first-sibling-bol "declare")
            ada-ts-mode-indent-broken-offset)
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "loop_label")
                 (matcher/node-is "begin"))
            parent
            ada-ts-mode-indent-label-offset)
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "begin")
                 (or (matcher/node-is "end")
                     (matcher/node-is ";")))
            (anchor/first-sibling-bol "begin")
            0)
           ((and (matcher/parent-is "block_statement")
                 (matcher/between-siblings-p "begin" "end"))
            (anchor/first-sibling-bol "begin")
            ada-ts-mode-indent-offset)
           ((and (matcher/parent-is "block_statement")
                 (matcher/after-first-sibling-p "begin")
                 (matcher/node-is "identifier"))
            (anchor/first-sibling-bol "begin")
            ada-ts-mode-indent-broken-offset)

           ;; keywords / semicolon
           ((node-is ,(eval `(rx bos (or ,@ada-ts-mode--keywords ";") eos))) parent-bol 0)
           ((or (matcher/node-is "elsif_statement_item")
                (matcher/node-is "aspect_specification")
                (matcher/node-is "null_exclusion")
                (matcher/node-is "access_to_object_definition")
                (matcher/node-is "access_to_subprogram_definition")
                (matcher/node-is "procedure_specification")
                (matcher/node-is "function_specification")
                (matcher/node-is "allocator"))
            parent-bol
            0)

           ;; select_statement
           ((and (or (matcher/node-is "select_alternative")
                     (matcher/node-is "comment"))
                 (matcher/prev-sibling-is "guard"))
            (anchor/prev-sibling "guard")
            ada-ts-mode-indent-offset)
           ((matcher/parent-is "selective_accept")       parent ada-ts-mode-indent-offset)
           ((matcher/parent-is "timed_entry_call")       parent ada-ts-mode-indent-offset)
           ((matcher/parent-is "conditional_entry_call") parent ada-ts-mode-indent-offset)
           ((matcher/parent-is "asynchronous_select")    parent ada-ts-mode-indent-offset)
           ((matcher/parent-is "accept_alternative")     parent 0)
           ((matcher/parent-is "delay_alternative")      parent 0)
           ((matcher/parent-is "entry_call_alternative") parent 0)
           ((matcher/parent-is "triggering_alternative") parent 0)

           ((or (and (or (matcher/parent-is "subprogram_body")
                         (matcher/parent-is "package_body")
                         (matcher/parent-is "package_declaration")
                         (matcher/parent-is "task_body")
                         (matcher/parent-is "entry_body")
                         (matcher/parent-is "protected_body"))
                     (matcher/between-siblings-p "is" "end"))
                (and (matcher/parent-is "extended_return_statement")
                     (matcher/between-siblings-p "do" "end"))
                (and (matcher/parent-is "if_statement")
                     (matcher/between-siblings-p "then" "end"))
                (and (matcher/parent-is "elsif_statement_item")
                     (matcher/after-first-sibling-p "then")))
            parent-bol
            ada-ts-mode-indent-offset)

           ;; general indentation for comments.
           ;;
           ;; NOTE: Indent to where next non-comment sibling would be
           ;; indented.  This may not be aligned to sibling if sibling isn't
           ;; properly indented, however it prevents a two-pass indentation
           ;; when region is indented, since comments won't have to be
           ;; reindented once sibling becomes properly aligned.
           ((and (matcher/node-is "comment")
                 (matcher/next-sibling-not-matching-exists-p "comment"))
            (anchor/next-sibling-not-matching "comment")
            (offset/next-sibling-not-matching "comment"))

           ;; identifier / selected_component
           ((or (matcher/node-is "identifier")
                (matcher/node-is "selected_component")
                (matcher/parent-is "selected_component"))
            parent-bol
            ada-ts-mode-indent-broken-offset)

           ;; non-expression opening parenthesis
           ((or (matcher/node-is "formal_part")
                (matcher/node-is "enumeration_aggregate")
                (matcher/node-is "enumeration_type_definition")
                (matcher/node-is "actual_parameter_part")
                (matcher/node-is "known_discriminant_part")
                (matcher/node-is "("))
            parent-bol
            ada-ts-mode-indent-broken-offset)

           ;; closing parenthesis (including expression)
           ((and (matcher/node-is ")")
                 (matcher/after-first-sibling-p "("))
            (anchor/first-sibling "(")
            0)
           ((and (matcher/node-is "]")
                 (matcher/after-first-sibling-p "["))
            (anchor/first-sibling "[")
            0)

           ;; miscellaneous punctuation
           ((or (matcher/node-is ":")
                (matcher/node-is ":="))
            parent
            0)
           ((matcher/node-is "=>") parent ada-ts-mode-indent-broken-offset)

           ;; If rule set is complete, this rule should never be matched.
           (catch-all (anchor/catch-all) (offset/catch-all))))))

;;; Indentation Verbosity

(defvar ada-ts-mode--indent-verbose nil
  "If non-nil, log process when indenting.")

(defun advice/treesit--indent-rules-optimize (oldfun &rest r)
  "Advice to prevent compiling tree-sitter queries.

OLDFUN is the original `treesit--indent-rules-optimize' function and R
are its called arguments.

Preventing the compilation of tree-sitter queries is necessary so that
the queries can be properly displayed when `treesit--indent-verbose' is
enabled rather than displaying `treesit-compiled-query', which is
unhelpful when debugging indentation rules."
  (if (and treesit--indent-verbose
           (derived-mode-p 'ada-ts-mode))
      (cl-letf (((symbol-function 'treesit-query-compile)
                 (lambda (_lang query &optional _eager)
                   (cond ((stringp query) query)
                         ((treesit-compiled-query-p query) query)
                         (t (treesit-query-expand query))))))
        (apply oldfun r))
    (apply oldfun r)))

(defun ada-ts-mode--indent-verbosity-config (symbol newval operation where)
  "Configure `ada-ts-mode' indent verbosity.

SYMBOL is expected to be `ada-ts-mode--indent-verbose', OPERATION is
queried to check that it is a `set' operation (as defined by
`add-variable-watcher'), and WHERE is queried to check that it is
nil (i.e., not buffer local), otherwise nothing is updated.

When SYMBOL is `ada-ts-mode--indent-verbose' and NEWVAL is non-nil,
rebuild indentation rules with string queries for easier debugging,
otherwise rebuild rules with compiled queries for performance."
  (when (and (eq operation 'set)
             (null where)
             (eq symbol 'ada-ts-mode--indent-verbose))
    (let ((recompute-rules
           (lambda ()
             ;; Recompute indent rules to compile/not compile queries due to
             ;; the removal/addition of the `treesit--indent-rules-optimize'
             ;; advice.
             (dolist (buffer (buffer-list))
               (with-current-buffer buffer
                 (when (derived-mode-p 'ada-ts-mode)
                   (message "Building %s indent queries for %s"
                            (if newval "uncompiled" "compiled")
                            (buffer-name))
                   (setq-local treesit-simple-indent-rules
                               (treesit--indent-rules-optimize
                                ada-ts-mode--indent-rules))))))))
      (cond (newval
             (setq treesit--indent-verbose t)
             (advice-add 'treesit--indent-rules-optimize
                         :around #'advice/treesit--indent-rules-optimize)
             (funcall recompute-rules))
            (ada-ts-mode--indent-verbose
             (setq treesit--indent-verbose nil)
             (advice-remove 'treesit--indent-rules-optimize
                            #'advice/treesit--indent-rules-optimize)
             (funcall recompute-rules))))))

(ada-ts-mode--indent-verbosity-config
 'ada-ts-mode--indent-verbose
 ada-ts-mode--indent-verbose
 'set nil)

(add-variable-watcher
 'ada-ts-mode--indent-verbose
 #'ada-ts-mode--indent-verbosity-config)

;;; Best-Effort Indentation

(defun ada-ts-mode--prev-leaf-node (start)
  "Find leaf node before START.

START is either a node or a position."
  (when-let* ((prev-node (ada-ts-mode--prev-node start)))
    (treesit-node-at
     (1- (treesit-node-end prev-node)))))

(defun ada-ts-mode--next-leaf-node (start)
  "Find leaf node after START.

START is either a node or a position."
  (when-let* ((next-node (ada-ts-mode--next-node start)))
    (treesit-node-at (treesit-node-start next-node))))

(defun ada-ts-mode--prev-token (start)
  "Find token before START.

START is either a node or a position."
  (when-let* ((prev-leaf-node (ada-ts-mode--prev-leaf-node start)))
    (treesit-node-type prev-leaf-node)))

(defun ada-ts-mode--matching-prev-node (start matches)
  "Find a node before START where node type is contained in MATCHES.

  MATCHES is either a string representing a node type, a list of strings
  representing node types or a predicate function which takes a node as
  its sole parameter and returns non nil for a match."
  (let ((prev-node start)
        (predicate (if (functionp matches)
                       matches
                     (lambda (node)
                       (member (treesit-node-type node)
                               (ensure-list matches))))))
    (while (or (treesit-node-eq prev-node start)
               (and prev-node
                    (not (funcall predicate prev-node))))
      (setq prev-node (ada-ts-mode--prev-node prev-node)))
    prev-node))

(defun ada-ts-mode--not-matching-prev-node (start matches)
  "Find a node before START where node type is not contained in MATCHES.

MATCHES is either a string representing a node type, a list of strings
representing node types or a predicate function which takes a node as
its sole parameter and returns non nil for a match."
  (let ((prev-node start)
        (predicate (if (functionp matches)
                       matches
                     (lambda (node)
                       (member (treesit-node-type node)
                               (ensure-list matches))))))
    (while (or (treesit-node-eq prev-node start)
               (and prev-node
                    (funcall predicate prev-node)))
      (setq prev-node (ada-ts-mode--prev-node prev-node)))
    prev-node))

(defun ada-ts-indent--node-at-indentation-p (node)
  "Check if NODE begins a line."
  (let* ((node-pos (treesit-node-start node))
         (indent-pos (save-excursion
                       (goto-char node-pos)
                       (back-to-indentation)
                       (point))))
    (= node-pos indent-pos)))

(defun ada-ts-indent--point-at-indentation (node)
  "Find position at indentation from start of NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun ada-ts-mode--is-keyword-anchor (node)
  "Find anchor node for \\='is\\=' keyword NODE."
  (when-let* ((anchor-node
               (ada-ts-mode--matching-prev-node
                node
                '("case" "entry"
                  "function" "function_specification"
                  "package"
                  "procedure" "procedure_specification"
                  "protected"
                  "subtype"
                  "task" "type")))
              (anchor-node-t (treesit-node-type anchor-node)))
    (when-let* (((string-equal anchor-node-t "type"))
                (prev-anchor-node (ada-ts-mode--prev-node anchor-node))
                (prev-anchor-node-t (treesit-node-type prev-anchor-node)))
      (when (member prev-anchor-node-t '("task" "protected"))
        (setq anchor-node prev-anchor-node
              anchor-node-t prev-anchor-node-t)))
    ;; Adjust to beginning of line to account for leading keywords
    ;; (such as "overriding" for functions and procedures) except for
    ;; "case", since if it's part of a case expression, it's part of a
    ;; larger expression or statement that we don't want to align
    ;; with.
    (if (string-equal anchor-node-t "case")
        anchor-node
      (treesit-node-at
       (ada-ts-indent--point-at-indentation anchor-node)))))

(defun ada-ts-mode--then-keyword-anchor (node)
  "Find anchor node for \\='then\\=' keyword NODE."
  (when-let* ((prev-node (ada-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (if (string-equal prev-node-t "and")
        (ada-ts-mode--prev-node prev-node)
      (ada-ts-mode--matching-prev-node node '("elsif" "#elsif" "if" "#if" "select")))))

(defun ada-ts-mode--do-keyword-anchor (node)
  "Find anchor node for \\='do\\=' keyword NODE."
  (ada-ts-mode--matching-prev-node node '("accept" "return")))

(defun ada-ts-indent--location-after-keyword-loop (node)
  "Find (anchor, offset) when after \\='loop\\=' keyword NODE."
  (when-let* ((prev-node (ada-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node))
              ;; Account for possible iteration_scheme
              (anchor-node (if (string-equal prev-node-t "iteration_scheme")
                               prev-node
                             node))
              ;; Account for possible loop_label
              (anchor (ada-ts-indent--point-at-indentation anchor-node)))
    (cons anchor ada-ts-mode-indent-offset)))

(defun ada-ts-indent--location-for-keyword-begin (node)
  "Find (anchor, offset) for \\='begin\\=' keyword NODE."
  (when-let* ((prev-node (ada-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (let ((anchor nil)
          (offset nil))
      (cond
       ;; "is" "begin"
       ((string-equal prev-node-t "is")
        (when-let* ((anchor-node (ada-ts-mode--is-keyword-anchor prev-node)))
          (setq anchor (treesit-node-start anchor-node)
                offset 0)))
       ;; "begin" "begin"
       ((string-equal prev-node-t "begin")
        (when-let* ((location (ada-ts-indent--location-after-keyword-begin prev-node)))
          (setq anchor (car location)
                offset (cdr location))))
       ;; loop_label "begin"
       ((string-equal prev-node-t "loop_label")
        (setq anchor (treesit-node-start prev-node)
              offset ada-ts-mode-indent-label-offset))
       ;; ["("] "declare" "begin"
       ((string-equal prev-node-t "declare")
        (setq anchor
              (if-let* ((prev-prev-node (ada-ts-mode--prev-node prev-node))
                        (prev-prev-node-t (treesit-node-type prev-prev-node))
                        ((string-equal prev-prev-node-t "(")))
                  ;; declare_expression
                  (treesit-node-start prev-node)
                ;; block_statement
                (ada-ts-indent--point-at-indentation prev-node))
              offset 0))
       ;; non_empty_declarative_part "begin"
       ((string-equal prev-node-t "non_empty_declarative_part")
        (when-let* ((prev-prev-node (ada-ts-mode--prev-node prev-node))
                    (prev-prev-node-t (treesit-node-type prev-prev-node)))
          (cond
           ;; "is" non_empty_declarative_part "begin"
           ((string-equal prev-prev-node-t "is")
            (let ((anchor-node (ada-ts-mode--is-keyword-anchor prev-prev-node)))
              (setq anchor (treesit-node-start anchor-node)
                    offset 0)))
           ;; "declare" non_empty_declarative_part "begin"
           ((string-equal prev-prev-node-t "declare")
            (setq anchor (ada-ts-indent--point-at-indentation prev-prev-node)
                  offset 0)))))
       ;; "(" "declare" [declare_item] "begin"
       ((member
         (treesit-node-type
          (ada-ts-mode--not-matching-prev-node node "pragma_g"))
         '("object_declaration" "object_renaming_declaration" "declare"))
        (setq anchor (treesit-node-start prev-node)
              offset (- ada-ts-mode-indent-offset)))
       (t
        (setq anchor (treesit-node-start prev-node)
              offset 0)))
      (cons anchor offset))))

(defun ada-ts-indent--location-after-keyword-begin (node)
  "Find (anchor, offset) when after \\='begin\\=' keyword NODE.

The offset is a list consisting of 1 or more offsets whose sum is the
total offset."
  (let ((anchor)
        (offset))
    (if (ada-ts-indent--node-at-indentation-p node)
        (setq anchor node
              offset ada-ts-mode-indent-offset)
      (if-let* ((prev-node (ada-ts-mode--prev-node node))
                (prev-node-t (treesit-node-type prev-node))
                ((string-equal prev-node-t "loop_label")))
          (setq anchor prev-node
                offset ada-ts-mode-indent-label-offset)
        (when-let* ((location (ada-ts-indent--location-for-keyword-begin node)))
          (setq anchor (car location)
                offset (cons ada-ts-mode-indent-offset (ensure-list (cdr location)))))))
    (when anchor
      (when (treesit-node-p anchor)
        (setq anchor (treesit-node-start anchor)))
      (cons anchor (ensure-list offset)))))

(defun ada-ts-indent--location-after-keyword-record (node)
  "Find (anchor, offset) when after \\='record\\=' keyword NODE."
  (when-let* ((prev-node (ada-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node))
              ((member prev-node-t '("is" "tagged" "limited" "use" "with"))))
    ;; Anchor to "limited" or "record" keyword if it's at the
    ;; beginning of the line, otherwise anchor to the "type"
    ;; (in type definition) or "for" (in representation
    ;; clause) keyword, which might be on a different line,
    ;; especially if discriminant arguments are present.
    (let* ((bol-pos (ada-ts-indent--point-at-indentation node))
           (bol-node (treesit-node-at bol-pos))
           (bol-node-t (treesit-node-type bol-node)))
      (if (member bol-node-t '("limited" "record"))
          (cons bol-pos ada-ts-mode-indent-offset)
        (when-let* ((anchor-node (ada-ts-mode--matching-prev-node node '("for" "type"))))
          (cons (treesit-node-start anchor-node)
                ada-ts-mode-indent-offset))))))

(defun ada-ts-mode--indent-best-effort (node _parent bol)
  "Attempt best effort to determine indentation of NODE at BOL."
  (when ada-ts-mode--indent-verbose
    (message "*** ORIG-NODE: %s" node))

  (setq node (treesit-node-at bol))
  (when (or (> (treesit-node-start node) bol)
            (<= (treesit-node-end node) bol))
    (setq node nil))

  (let* ((node-t (treesit-node-type node))
         (prev-node (ada-ts-mode--prev-node bol))
         (prev-node-t (treesit-node-type prev-node))
         anchor offset scenario)
    (when ada-ts-mode--indent-verbose
      (message "*** NODE: %s" node)
      (message "*** BOL: %s" bol)
      (message "*** PREV-NODE: %s" prev-node))
    (unless prev-node
      (setq anchor (point-min)
            offset 0
            scenario "Scenario [No previous node]"))
    ;; Keyword: "is"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "is")))
                (anchor-node (ada-ts-mode--is-keyword-anchor node)))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: 'is']"))
    ;; Keyword: "end"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "end")))
                (anchor-node (ada-ts-mode--matching-prev-node
                              node
                              '("accept" "begin" "case" "if" "loop" "package"
                                "protected" "record" "return" "select" "task")))
                (anchor-node-t (treesit-node-type anchor-node)))
      (if (ada-ts-indent--node-at-indentation-p anchor-node)
          (setq anchor (treesit-node-start anchor-node)
                offset 0)
        (cond
         ((string-equal anchor-node-t "begin")
          (when-let* ((location (ada-ts-indent--location-after-keyword-begin anchor-node)))
            (setq anchor (car location)
                  offset (cons 0 (cddr location)))))
         ((string-equal anchor-node-t "record")
          (setq anchor (car (ada-ts-indent--location-after-keyword-record anchor-node))
                offset 0))
         ((string-equal anchor-node-t "loop")
          (setq anchor (car (ada-ts-indent--location-after-keyword-loop anchor-node))
                offset 0))
         (t
          (setq anchor (ada-ts-indent--point-at-indentation anchor-node)
                offset 0))))
      (when anchor
        (setq scenario "Scenario [Keyword: 'end']")))
    ;; Keyword: "#end"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "#end")))
                (anchor-node (ada-ts-mode--matching-prev-node
                              node
                              '("#if" "#elsif" "#else"))))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: '#end']"))
    ;; Keyword: "private"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "private")))
                (anchor-node (ada-ts-mode--matching-prev-node node '("package" "type" "task" "protected")))
                (anchor-node-t (treesit-node-type anchor-node)))
      (setq anchor
            (save-excursion
              (goto-char (treesit-node-start anchor-node))
              (back-to-indentation)
              (point))
            offset 0
            scenario "Scenario [Keyword: 'private']"))
    ;; Keyword: "begin"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "begin")))
                (location (ada-ts-indent--location-for-keyword-begin node)))
      (setq anchor (car location)
            offset (cdr location)
            scenario "Scenario [Keyword: 'begin']"))
    ;; Keyword: "limited"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "limited")))
                (next-node (ada-ts-mode--next-leaf-node node))
                (next-node-t (treesit-node-type next-node))
                ((string-equal next-node-t "record"))
                (anchor-node (ada-ts-mode--matching-prev-node node "type")))
      (setq anchor (treesit-node-start anchor-node)
            offset ada-ts-mode-indent-record-offset
            scenario "Scenario [Keyword: 'limited']"))
    ;; Keyword: "record"
    (when-let* (((not anchor))
                ((and node-t
                      (member node-t '("record" "record_definition"))))
                ((member prev-node-t '("is" "tagged" "limited" "use" "with")))
                (anchor-node (ada-ts-mode--matching-prev-node node '("type" "for"))))
      (setq anchor (treesit-node-start anchor-node)
            offset ada-ts-mode-indent-record-offset
            scenario "Scenario [Keyword: 'record']"))
    ;; After Keyword: "record"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "record"))
                (location (ada-ts-indent--location-after-keyword-record prev-node)))
      (setq anchor (car location)
            offset (cdr location)
            scenario "Scenario [After Keyword: 'record']"))
    ;; After Keyword: "is"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "is"))
                (anchor-node (ada-ts-mode--is-keyword-anchor prev-node))
                (anchor-node-s (treesit-node-start anchor-node))
                (anchor-node-t (treesit-node-type anchor-node)))
      (setq anchor anchor-node-s
            offset (if (string-equal anchor-node-t "case")
                       ada-ts-mode-indent-when-offset
                     ada-ts-mode-indent-offset)
            scenario "Scenario [After Keyword: 'is']"))
    ;; Keyword: "#elsif", "#else"
    (when-let* (((not anchor))
                ((and node-t
                      (member node-t '("#elsif" "#else"))))
                (anchor-node (ada-ts-mode--matching-prev-node
                              node
                              '("#if" "#elsif"))))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: '#elsif', '#else']"))
    ;; Keyword: "elsif", "else"
    (when-let* (((not anchor))
                ((and node-t
                      (member node-t '("elsif" "else"))))
                (anchor-node (ada-ts-mode--matching-prev-node
                              node
                              '("if_expression"
                                "elsif_expression_item"
                                "if_statement"
                                "elsif_statement_item"
                                "if" "elsif" "select")))
                (anchor-node-s (treesit-node-start anchor-node)))
      (setq anchor anchor-node-s
            offset 0
            scenario "Scenario [Keyword: 'elsif', 'else']"))
    ;; After Keyword: "#else"
    (when-let* (((not anchor))
                ((and prev-node-t (string-equal prev-node-t "#else"))))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keyword: '#else']"))
    ;; Keyword: "exception"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "exception")))
                ((and prev-node-t (not (string-equal prev-node-t ":"))))
                (anchor-node (ada-ts-mode--matching-prev-node
                              node '("begin" "do")))
                (anchor-node-t (treesit-node-type anchor-node)))
      (cond
       ;; "begin" ... "exception"
       ((string-equal anchor-node-t "begin")
        (when-let* ((location (ada-ts-indent--location-after-keyword-begin anchor-node)))
          (setq anchor (car location)
                offset (cons 0 (cddr location)))))
       ;; "do" ... "exception"
       ((string-equal anchor-node-t "do")
        (setq anchor-node (ada-ts-mode--do-keyword-anchor anchor-node))
        (when anchor-node
          (setq anchor (treesit-node-start anchor-node)
                offset 0))))
      (when anchor
        (setq scenario "Scenario [Keyword: 'exception']")))
    ;; Keyword: "then"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "then")))
                (anchor-node (ada-ts-mode--then-keyword-anchor node)))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: 'then']"))
    ;; After Keyword: "then"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "then"))
                (anchor-node (ada-ts-mode--then-keyword-anchor prev-node))
                (anchor-node-t (treesit-node-type anchor-node))
                ((member anchor-node-t '("elsif" "#elsif" "if" "#if"))))
      (setq anchor (treesit-node-start anchor-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keyword 'then']"))
    ;; Keyword: "do"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "do")))
                (anchor-node (ada-ts-mode--do-keyword-anchor node)))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: 'do']"))
    ;; After Keyword: "do"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "do"))
                (anchor-node (ada-ts-mode--do-keyword-anchor prev-node))
                (anchor-node-s (treesit-node-start anchor-node)))
      (setq anchor anchor-node-s
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keyword: 'do']"))
    ;; After "loop_label"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "loop_label")))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-label-offset
            scenario "Scenario [After 'loop_label']"))
    ;; Keyword: "loop"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "loop")))
                ((not (string-equal prev-node-t "end"))))
      (setq anchor
            ;; Account for possible iteration_scheme
            (ada-ts-indent--point-at-indentation prev-node)
            offset (if (string-equal prev-node-t "begin")
                       ada-ts-mode-indent-offset
                     0)
            scenario "Scenario [Keyword: 'loop']"))
    ;; After Keyword: "loop"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "loop"))
                (location (ada-ts-indent--location-after-keyword-loop prev-node)))
      (setq anchor (car location)
            offset (cdr location)
            scenario "Scenario [After Keyword: 'loop']"))
    ;; Generic subprogram/package declaration
    (when-let* (((not anchor))
                ((and node-t
                      (member node-t '("function"  "function_specification"
                                       "package"   "package_declaration"
                                       "procedure" "procedure_specification"))))
                ((member prev-node-t '("generic" "generic_formal_part"))))
      (setq anchor (treesit-node-start prev-node)
            offset 0
            scenario "Scenario [Generic subprogram/package declaration]"))
    ;; Keywords after expression: "or", "and"
    (when-let* (((not anchor))
                ((and node-t (member node-t '("or" "and"))))
                ((and prev-node-t (string-equal prev-node-t "expression"))))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-exp-item-offset
            scenario "Scenario [Keywords after expression: 'or', 'and']"))
    ;; Keywords: "or", "else" (select statement)
    (when-let* (((not anchor))
                ((and node-t (member node-t '("or" "else"))))
                (prev-token (ada-ts-mode--prev-token node))
                ((string-equal prev-token ";"))
                (anchor-node (ada-ts-mode--matching-prev-node node "select")))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keywords: 'or', 'else']"))
    ;; After Keyword: "or" (select statement)
    (when-let* (((not anchor))
                ((string-equal prev-node-t "or"))
                (prev-prev-token (ada-ts-mode--prev-token prev-node))
                ((string-equal prev-prev-token ";")))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keyword: 'or']"))
    ;; After Keyword: "abort" (select statement)
    (when-let* (((not anchor))
                ((string-equal prev-node-t "abort"))
                (prev-prev-token (ada-ts-mode--prev-token prev-node))
                ((string-equal prev-prev-token "then"))
                (anchor-node (ada-ts-mode--matching-prev-node prev-node "select")))
      (setq anchor (treesit-node-start anchor-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keyword: 'abort']"))
    ;; After ";", "," and "|"
    (when-let* (((not anchor))
                ((member prev-node-t '(";" "," "|")))
                (prev-prev-node (ada-ts-mode--prev-node prev-node)))
      (setq anchor (treesit-node-start prev-prev-node)
            offset 0
            scenario "Scenario [After ';', ',', and '|']"))
    ;; After Punctuation: "=>"
    (when-let* (((not anchor))
                (prev-leaf-node (ada-ts-mode--prev-leaf-node bol))
                (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                ((string-equal prev-leaf-node-t "=>"))
                (anchor-node (ada-ts-mode--matching-prev-node prev-leaf-node '("when" "," "(")))
                (anchor-node-t (treesit-node-type anchor-node)))
      (if (string-equal anchor-node-t "when")
          (setq anchor (treesit-node-start anchor-node)
                offset ada-ts-mode-indent-offset
                scenario "Scenario [After Punctuation: '=>']")
        (when (setq anchor-node (ada-ts-mode--next-node anchor-node))
          (setq anchor (treesit-node-start anchor-node)
                offset ada-ts-mode-indent-broken-offset
                scenario "Scenario [After Punctuation: '=>']"))))
    ;; Punctuation: "("
    (when-let* (((not anchor))
                ((and node (string-equal node-t "(")))
                (prev-leaf-node (ada-ts-mode--prev-leaf-node node))
                (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                ((string-equal prev-leaf-node-t "identifier"))
                (anchor-node (treesit-parent-while
                              prev-leaf-node
                              (lambda (node)
                                (member (treesit-node-type node) '("identifier" "selected_component"))))))
      (setq anchor (ada-ts-indent--point-at-indentation anchor-node)
            offset ada-ts-mode-indent-broken-offset
            scenario "Scenario [Punctuation: '(']"))
    ;; Newline after shebang
    (when-let* (((not anchor))
                ((not node))
                (prev-leaf-node (ada-ts-mode--prev-leaf-node bol))
                (bol-pos (ada-ts-indent--point-at-indentation prev-leaf-node))
                ((save-excursion
                   (goto-char bol-pos)
                   (looking-at-p (rx "#!")))))
      (setq anchor bol-pos
            offset 0
            scenario "Scenario [Newline after shebang]"))
    ;; Newline after identifer/selected_component
    (when-let* (((not anchor))
                ((not node))
                (prev-leaf-node (ada-ts-mode--prev-leaf-node (point)))
                (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                ((string-equal prev-leaf-node-t "identifier"))
                (anchor-node (treesit-parent-while
                              prev-leaf-node
                              (lambda (node)
                                (member (treesit-node-type node) '("identifier" "selected_component"))))))
      (setq anchor (ada-ts-indent--point-at-indentation anchor-node)
            offset ada-ts-mode-indent-broken-offset
            scenario "Scenario [Newline after identifier/selected_component"))
    ;; Newline after case_statement_alternative
    (when-let* (((not anchor))
                ((not node))
                ((string-equal prev-node-t "case_statement_alternative")))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [Newline after case_statement_alternative]"))
    ;; Newline after variant/variant_list
    (when-let* (((not anchor))
                ((not node))
                ((member prev-node-t '("variant" "variant_list"))))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [Newline after variant/variant_list]"))
    ;; Newline after handled_sequence_of_statements
    (when-let* (((not anchor))
                ((not node))
                ((string-equal prev-node-t "handled_sequence_of_statements"))
                (last-pos (1- (treesit-node-end prev-node)))
                (last-node (treesit-node-at last-pos))
                (statement-node
                 (treesit-parent-until
                  last-node
                  (lambda (node)
                    (ada-ts-mode--statement-p node))
                  'include-node)))
      ;; Anchor to beginning of line to handle multiple items per line
      ;; (e.g., "null; null;")
      (setq anchor (ada-ts-indent--point-at-indentation statement-node)
            offset 0
            scenario "Scenario [Newline after handled_sequence_of_statements]"))
    ;; After Punctuation: ":="
    (when-let* (((not anchor))
                ((string-equal prev-node-t ":="))
                (prev-anchor-node (ada-ts-mode--matching-prev-node
                                   prev-node
                                   (lambda (node)
                                     (let ((node-t (treesit-node-type node)))
                                       (or (member node-t '("begin" "is" "(" ";"))
                                           (string-equal
                                            (treesit-node-type
                                             (treesit-node-at (1- (treesit-node-end node))))
                                            ";"))))))
                (anchor-node (ada-ts-mode--next-node prev-anchor-node)))
      (setq anchor (treesit-node-start anchor-node)
            offset ada-ts-mode-indent-broken-offset
            scenario "Scenario [After Punctuation: ':=']"))
    ;; After Keyword: declare
    (when-let* (((not anchor))
                ((string-equal prev-node-t "declare")))
      (setq anchor
            (if-let* ((prev-prev-node (ada-ts-mode--prev-node prev-node))
                      (prev-prev-node-t (treesit-node-type prev-prev-node))
                      ((string-equal prev-prev-node-t "(")))
                ;; declare_expression
                (treesit-node-start prev-node)
              ;; block_statement
              (ada-ts-indent--point-at-indentation prev-node))
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keyword: declare]"))
    ;; After Keyword: begin
    (when-let* (((not anchor))
                ((and prev-node (string-equal prev-node-t "begin")))
                (location (ada-ts-indent--location-after-keyword-begin prev-node)))
      (setq anchor (car location)
            offset (cdr location)
            scenario "Scenario [After Keyword: begin]"))
    ;; After Keywords: ada-ts-mode-indent-offset
    (when-let* (((not anchor))
                ((member prev-node-t '("else" "exception" "generic" "private" "record" "select"))))
      (setq anchor (ada-ts-indent--point-at-indentation prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After Keywords: ada-ts-mode-indent-offset]"))
    ;; After elsif_statement_item
    (when-let* (((not anchor))
                ((string-equal prev-node-t "elsif_statement_item")))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [After elsif_statement_item]"))
    ;; After keywords
    (when-let* (((not anchor))
                ((member prev-node-t ada-ts-mode--keywords)))
      (setq anchor (ada-ts-indent--point-at-indentation prev-node)
            offset (if (and node
                            (member
                             (treesit-node-type (treesit-node-at (treesit-node-start node)))
                             ada-ts-mode--keywords))
                       0 ;; Adjacent keywords (e.g., "overriding function")
                     ada-ts-mode-indent-broken-offset)
            scenario "Scenario [After keywords]"))
    ;; Previous Punctuation: "("
    (when-let* (((not anchor))
                ((string-equal prev-node-t "(")))
      (setq anchor (treesit-node-start prev-node)
            offset 1
            scenario "Scenario [Previous Punctuation: '(']"))
    ;; After Goto Label
    (when-let* (((not anchor))
                ((string-equal prev-node-t "label")))
      (setq anchor (treesit-node-start prev-node)
            offset 0
            scenario "Scenario [After Goto Label]"))
    ;; After Compilation Unit / Declaration / Statement
    (when-let* (((not anchor))
                ((or (string-equal prev-node-t "compilation_unit")
                     (ada-ts-mode--compilation-unit-p prev-node))))
      ;; Anchor to beginning of line to handle multiple items per line
      ;; (e.g., "with System; use System;")
      (setq anchor (ada-ts-indent--point-at-indentation prev-node)
            offset 0
            scenario "Scenario [After Compilation Unit / Declaration / Statement]"))
    ;; After shebang
    (when-let* (((not anchor))
                (prev-leaf-node (ada-ts-mode--prev-leaf-node bol))
                (bol-pos (ada-ts-indent--point-at-indentation prev-leaf-node))
                ((save-excursion
                   (goto-char bol-pos)
                   (looking-at-p (rx "#!")))))
      (setq anchor bol-pos
            offset 0
            scenario "Scenario [After shebang]"))
    ;; Fallback
    (unless anchor
      (setq scenario "Scenario [fallback]")
      (if (string-equal
           (treesit-node-type
            (treesit-node-at (1- (treesit-node-end prev-node))))
           ";")
          (setq anchor (treesit-node-start prev-node)
                offset 0)
        (setq anchor (treesit-node-start prev-node)
              offset ada-ts-mode-indent-broken-offset)))
    (when ada-ts-mode--indent-verbose
      (message scenario))
    (cons anchor (apply #'+ (ensure-list offset)))))

;;; Indentation Matchers

(defun matcher/-first-child-matching (parent type)
  "Find first child of PARENT matching TYPE.
Return nil if no child of that type is found."
  ;; NOTE: `treesit-filter-child' uses `treesit-node-next-sibling'
  ;; which doesn't traverse parser-inserted "missing" nodes (seems
  ;; like a bug), so filter the nodes manually.
  (seq-find
   (lambda (n)
     (string-equal (treesit-node-type n) type))
   (treesit-node-children parent)))

(defun matcher/parent-is (type)
  "Determine if the parent node type is TYPE."
  (lambda (_node parent &rest _)
    (and parent
         (string-equal (treesit-node-type parent) type))))

(defun matcher/node-is (type)
  "Determine if the node type is TYPE."
  (lambda (node &rest _)
    (and node
         (string-equal (treesit-node-type node) type))))

(defun matcher/field-is (name)
  "Determine if the node's field name is NAME."
  (lambda (node &rest _)
    (when-let* ((field-name (treesit-node-field-name node)))
      (string-equal field-name name))))

(defun matcher/first-child-is (type)
  "Determine if the node's first child node type is TYPE."
  (lambda (node _parent &rest _)
    (when-let* (((and node))
                (child (treesit-node-child node 0))
                (child-t (treesit-node-type child)))
      (string-equal child-t type))))

(defun matcher/first-sibling-is (type)
  "Determine if the parent node's first child node type is TYPE."
  (lambda (_node parent &rest _)
    (when-let* (((and parent))
                (child (treesit-node-child parent 0))
                (child-t (treesit-node-type child)))
      (string-equal child-t type))))

(defun matcher/-prev-sibling (node parent bol &rest _)
  "Determine previous sibling in PARENT before this NODE or BOL."
  (if node
      (treesit-node-prev-sibling node)
    (car
     (reverse
      (treesit-filter-child
       parent
       (lambda (n)
         (< (treesit-node-start n) bol)))))))

(defun matcher/prev-sibling-is (type)
  "Determine if the previous sibling node's type matches TYPE.

Comment nodes are ignored."
  (lambda (node parent bol &rest _)
    (let ((prev (matcher/-prev-sibling node parent bol)))
      (while (and prev (string-equal (treesit-node-type prev) "comment"))
        (setq prev (treesit-node-prev-sibling prev)))
      (when prev
        (string-equal (treesit-node-type prev) type)))))

(defun matcher/after-first-sibling-p (sibling)
  "Determine if the location of node comes after SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
               (matcher/-first-child-matching parent sibling)))
        (< (treesit-node-start sibling-node) bol))))

(defun matcher/before-first-sibling-p (sibling)
  "Determine if the location of node comes before SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
               (matcher/-first-child-matching parent sibling)))
        (> (treesit-node-start sibling-node) bol)
      ;; if it doesn't exist, node is before it
      t)))

(defun matcher/between-siblings-p (first-sibling last-sibling)
  "Determine if node is between FIRST-SIBLING and LAST-SIBLING."
  (lambda (node parent bol &rest _)
    (let ((after (matcher/after-first-sibling-p first-sibling))
          (before (matcher/before-first-sibling-p last-sibling)))
      (and (funcall after node parent bol)
           (funcall before node parent bol)))))

(defun ada-ts-mode--next-sibling-not-matching (type &rest types)
  "Locate next sibling not matching TYPE or TYPES."
  (lambda (node _parent _bol &rest _)
    (let ((all-types (cons type types))
          (sibling-node (treesit-node-next-sibling node)))
      (while (and sibling-node
                  (seq-some (lambda (a-type)
                              (equal (treesit-node-type sibling-node) a-type))
                            all-types))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'matcher/next-sibling-not-matching-exists-p 'ada-ts-mode--next-sibling-not-matching)

;;; Indentation Anchors and Offsets

(defvar-local ada-ts-mode--indent-best-effort-cache nil)

(defun anchor/best-effort ()
  "Determine best-effort anchor."
  (lambda (node parent bol &rest _)
    (if (and ada-ts-mode--indent-best-effort-cache
             (eq (buffer-chars-modified-tick) (plist-get ada-ts-mode--indent-best-effort-cache :tick))
             (treesit-node-eq node (plist-get ada-ts-mode--indent-best-effort-cache :node))
             (eq bol (plist-get ada-ts-mode--indent-best-effort-cache :bol)))
        (let ((anchor (plist-get ada-ts-mode--indent-best-effort-cache :anchor)))
          (when ada-ts-mode--indent-verbose
            (message "Anchor (Cached): %s" anchor))
          anchor)
      (let* ((results (ada-ts-mode--indent-best-effort node parent bol))
             (anchor (car results))
             (offset (cdr results)))
        (setq ada-ts-mode--indent-best-effort-cache
              (list :tick (buffer-chars-modified-tick)
                    :node node
                    :bol bol
                    :anchor anchor
                    :offset offset))
        (when ada-ts-mode--indent-verbose
          (message "Anchor: %s" anchor))
        anchor))))

(defun offset/best-effort ()
  "Determine best-effort offset."
  (lambda (node parent bol &rest _)
    (if (and ada-ts-mode--indent-best-effort-cache
             (eq (buffer-chars-modified-tick) (plist-get ada-ts-mode--indent-best-effort-cache :tick))
             (treesit-node-eq node (plist-get ada-ts-mode--indent-best-effort-cache :node))
             (eq bol (plist-get ada-ts-mode--indent-best-effort-cache :bol)))
        (let ((offset (plist-get ada-ts-mode--indent-best-effort-cache :offset)))
          (when ada-ts-mode--indent-verbose
            (message "Offset (Cached): %s" offset))
          offset)
      (let* ((results (ada-ts-mode--indent-best-effort node parent bol))
             (anchor (car results))
             (offset (cdr results)))
        (setq ada-ts-mode--indent-best-effort-cache
              (list :tick (buffer-chars-modified-tick)
                    :node node
                    :bol bol
                    :anchor anchor
                    :offset offset))
        (when ada-ts-mode--indent-verbose
          (message "Offset: %s" offset))
        offset))))

;; NOTE: This function is overridden in the test harness to detect if
;; an indentation test attempts to use a "catch-all" rule, which is an
;; indication of a missing formal rule.
(defun anchor/catch-all ()
  "Determine catch-all anchor."
  (anchor/best-effort))

(defun offset/catch-all ()
  "Determine catch-all offset."
  (offset/best-effort))

(defun anchor/first-sibling (type &rest types)
  "Determine BOL anchor for first sibling matching TYPE.

If TYPES is provided, then match the first sibling whose type matches
any of the types in TYPE or TYPES."
  (let ((all-types (cons type types)))
    (lambda (_node parent &rest _)
      (when-let* ((sibling-node
                   (car
                    (treesit-filter-child
                     parent
                     (lambda (n)
                       (member (treesit-node-type n) all-types))))))
        (treesit-node-start sibling-node)))))

(defun anchor/first-sibling-bol (type &rest types)
  "Determine BOL anchor for first sibling matching TYPE.

If TYPES is provided, then match the first sibling whose type matches
any of the types in TYPE or TYPES."
  (let ((all-types (cons type types)))
    (lambda (_node parent &rest _)
      (when-let* ((sibling-node
                   (car
                    (treesit-filter-child
                     parent
                     (lambda (n)
                       (member (treesit-node-type n) all-types))))))
        (save-excursion
          (goto-char (treesit-node-start sibling-node))
          (back-to-indentation)
          (point))))))

(defun anchor/prev-sibling (type)
  "Locate previous sibling matching TYPE."
  (lambda (_node parent bol &rest _)
    (treesit-node-start
     (car
      (last
       (treesit-filter-child
        parent
        (lambda (n)
          (and
           (equal (treesit-node-type n) type)
           (< (treesit-node-start n) bol)))))))))

(defun anchor/gp-skip-label-bol ()
  "Determine BOL anchor for first non-label child of grand-parent."
  (lambda (_node parent _bol &rest _)
    (let* ((gp-node (treesit-node-parent parent))
           (non-label-node
            (car
             (treesit-filter-child
              gp-node
              (lambda (n)
                (not (string-equal (treesit-node-type n) "loop_label")))))))
      (save-excursion
        (goto-char (treesit-node-start non-label-node))
        (back-to-indentation)
        (point)))))

(defun anchor/next-sibling-not-matching (type &rest types)
  "Determine indentation anchor of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'ada-ts-mode--next-sibling-not-matching all-types) node parent bol)))
      (car (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun offset/next-sibling-not-matching (type &rest types)
  "Determine indentation offset of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'ada-ts-mode--next-sibling-not-matching all-types) node parent bol)))
      (cdr (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

;;; Indent Line / Indent Region

(defvar-local ada-ts-indent--last-indent-tick nil)

(defun ada-ts-mode--indent-line ()
  "Perform line indentation."
  (prog1
      (ada-ts-mode-indent-line ada-ts-mode-indent-backend)
    (setq ada-ts-indent--last-indent-tick (buffer-chars-modified-tick))))

(defun ada-ts-mode--indent-region (beg end)
  "Perform region indentation between BEG and END."
  (prog1
      (ada-ts-mode-indent-region ada-ts-mode-indent-backend beg end)
    (setq ada-ts-indent--last-indent-tick (buffer-chars-modified-tick))))

(cl-defgeneric ada-ts-mode-indent-line (backend)
  "Indent line using BACKEND."
  (error "Unknown indentation backend: %s" backend))

(cl-defgeneric ada-ts-mode-indent-region (backend _beg _end)
  "Indent region between BEG and END using BACKEND."
  (error "Unknown indentation backend: %s" backend))

;;;; LSP

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql lsp)))
  "Indent line using LSP server BACKEND.

If LSP line formatting fails, fallback on tree-sitter indentation."
  (or (als/format-line ada-ts-mode-indent-offset)
      (ada-ts-mode-indent-line 'tree-sitter)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql lsp)) beg end)
  "Indent the region between BEG and END using LSP server BACKEND.

If LSP region formatting fails, fallback on tree-sitter indentation."
  (or (als/format-region beg end ada-ts-mode-indent-offset)
      (ada-ts-mode-indent-region 'tree-sitter beg end)))

;;;; Tree-sitter

(cl-defgeneric ada-ts-mode-indent (strategy)
  "Indent using tree-sitter back-end, according to STRATEGY."
  (error "Unknown indentation strategy: %s" strategy))

(cl-defmethod ada-ts-mode-indent ((_strategy (eql line)))
  "Indent using tree-sitter back-end, according to line STRATEGY."
  (treesit-indent))

(defun ada-ts-indent--aggressively-p (node)
  "Determine whether NODE should be indented aggressively.

This predicate checks that the NODE does not exceed 100 lines of source."
  (let* ((line-start (line-number-at-pos (treesit-node-start node)))
         (line-end (line-number-at-pos (treesit-node-end node))))
    (<= (- line-end line-start) 100)))

(defvar ada-ts-indent-aggressively-predicate #'ada-ts-indent--aggressively-p
  "Provides a predicate which is used to determine whether to indent aggressively.

The intention is to impose a performance metric check prior to
performing the indentation.  The aggressive indentation strategy must
have already been configured in `ada-ts-mode-indent-strategy'.  Even
when configured, other checks are performed to determine whether an
aggressive indentation is actually initiated.  This predicate is used to
impose a performance metric check, such that indentation does not appear
sluggish to the user.

For example, in very large files, the node to aggressively indent may be
extremely large and therefore aggressive indentation is undesireable, as
it would make the editor appear sluggish to the user.  In such a case
the predicate could check the number of lines of the node and make a
determination to not aggressively indent.

When nil or set to a predicate such as `always', the performance metric
is considered to always be met.")

(cl-defmethod ada-ts-mode-indent ((_strategy (eql aggressive)))
  "Indent using tree-sitter back-end, according to aggressive STRATEGY."
  (let ((initial-point-column (current-column))
        (initial-indentation-column (current-indentation))
        (region
         (save-excursion
           (forward-line 0)
           (skip-chars-forward " \t")
           (unless (looking-at (rx (* whitespace) eol) t)
             (let* ((node (treesit-node-at (point)))
                    (node-t (treesit-node-type node)))
               (if (string-equal node-t "comment")
                   ;; Expand to adjacent comment blocks
                   (let ((prev-node node)
                         (prev-node-t node-t)
                         (next-node node)
                         (next-node-t node-t)
                         start-node end-node)
                     (while (and prev-node-t (string-equal prev-node-t "comment")
                                 ;; Don't expand to include trailing comments
                                 (save-excursion
                                   (goto-char (treesit-node-start prev-node))
                                   (back-to-indentation)
                                   (treesit-node-eq (treesit-node-at (point)) prev-node)))
                       (setq start-node prev-node
                             prev-node (ada-ts-mode--prev-node start-node 'include-comments)
                             prev-node-t (treesit-node-type prev-node)))
                     (while (and next-node-t (string-equal next-node-t "comment"))
                       (setq end-node next-node
                             next-node (ada-ts-mode--next-node end-node 'include-comments)
                             next-node-t (treesit-node-type next-node)))
                     (when ada-ts-mode--indent-verbose
                       (message "Aggressive indent triggered for: (%s %s)" start-node end-node))
                     (cons (treesit-node-start start-node)
                           (treesit-node-end end-node)))
                 (let* ((root (treesit-buffer-root-node))
                        (candidate
                         (treesit-parent-until
                          node
                          (lambda (node)
                            (or (treesit-node-eq node root)
                                (string-equal (treesit-node-type node) "ERROR")
                                (ada-ts-mode--compilation-unit-p node)))
                          'include-node)))
                   (when (and (ada-ts-mode--compilation-unit-p candidate)
                              (or (not (functionp ada-ts-indent-aggressively-predicate))
                                  (funcall ada-ts-indent-aggressively-predicate candidate))
                              (not (treesit-search-subtree
                                    candidate
                                    (lambda (n)
                                      (let ((type (treesit-node-type n)))
                                        (or (string-equal type "ERROR")
                                            (treesit-node-check n 'missing)
                                            (treesit-node-check n 'has-error)))))))
                     (unless (ada-ts-mode--mismatched-names-p candidate)
                       (when ada-ts-mode--indent-verbose
                         (message "Aggressive indent triggered for: %s" candidate))
                       (cons (treesit-node-start candidate)
                             (treesit-node-end candidate)))))))))))
    (if region
        (progn
          (treesit-indent-region (car region) (cdr region))
          ;; Move point if it was in the indentation.
          (when (<= initial-point-column
                    initial-indentation-column)
            (back-to-indentation)))
      (treesit-indent))))

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql tree-sitter)))
  "Indent line using tree-sitter BACKEND."
  (if (eq ada-ts-mode-indent-backend 'tree-sitter)
      ;; Only utilize indentation strategy when tree-sitter back-end
      ;; is configured, not when tree-sitter back-end is used as a
      ;; fallback, to avoid competing indentation styles.
      (ada-ts-mode-indent ada-ts-mode-indent-strategy)
    (ada-ts-mode-indent 'line)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql tree-sitter)) beg end)
  "Indent the region between BEG and END using tree-sitter BACKEND."
  (treesit-indent-region beg end))

(defun ada-ts-mode--mismatched-names-p (node)
  "Determine if NODE names are mismatched."

  (let ((node-t (treesit-node-type node)))
    (cond ((string-equal node-t "object_declaration")
           (when-let* ((first-child-node (treesit-node-child node 0))
                       (first-child-node-t (treesit-node-type first-child-node))
                       ((member first-child-node-t
                                '("single_protected_declaration"
                                  "single_task_declaration"))))
             (setq node first-child-node)))
          ((string-equal node-t "full_type_declaration")
           (when-let* ((first-child-node (treesit-node-child node 0))
                       (first-child-node-t (treesit-node-type first-child-node))
                       ((member first-child-node-t
                                '("protected_type_declaration"
                                  "task_type_declaration"))))
             (setq node first-child-node)))))

  (catch 'not-applicable
    (let* ((node-t (treesit-node-type node))
           (name
            (cond ((member node-t
                           '("entry_body"
                             "package_body"
                             "package_declaration"
                             "protected_body"
                             "protected_type_declaration"
                             "single_protected_declaration"
                             "single_task_declaration"
                             "subprogram_body"
                             "task_body"
                             "task_type_declaration"))
                   (ada-ts-mode--defun-name node 'no-property))
                  ((string-equal node-t "block_statement")
                   (let ((first-child-node (treesit-node-child node 0)))
                     (when (string-equal (treesit-node-type first-child-node) "loop_label")
                       (ada-ts-mode--node-to-name
                        (treesit-node-child-by-field-name first-child-node "statement_identifier")
                        'no-property))))
                  (t (throw 'not-applicable nil))))
           (endname
            (let ((node node))
              (when (member node-t
                            '("protected_type_declaration"
                              "single_protected_declaration"))
                (setq node
                      (car (treesit-filter-child
                            node
                            (lambda (node)
                              (string-equal
                               (treesit-node-type node)
                               "protected_definition"))))))
              (when (member node-t
                            '("single_task_declaration"
                              "task_type_declaration"))
                (setq node
                      (car (treesit-filter-child
                            node
                            (lambda (node)
                              (string-equal
                               (treesit-node-type node)
                               "task_definition"))))))
              (when-let* ((end-node (car (treesit-filter-child
                                          node
                                          (lambda (node)
                                            (string-equal
                                             (treesit-node-type node)
                                             "end")))))
                          (next-node (ada-ts-mode--next-node end-node))
                          (next-node-t (treesit-node-type next-node))
                          ((member next-node-t '("identifier" "selected_component"))))
                (ada-ts-mode--node-to-name next-node 'no-property)))))
      (when ada-ts-mode--indent-verbose
        (message "NAME: %s" name)
        (message "ENDNAME: %s" endname))
      (or (not name)
          (not endname)
          (not (string-equal-ignore-case name endname))))))

;;; Node Predicates

(defun ada-ts-mode--compilation-unit-p (node)
  "Determine if NODE is a compilation unit."
  (or (ada-ts-mode--declarative-item-p node)
      (ada-ts-mode--statement-p node)
      (let ((node-type (treesit-node-type node)))
        (member node-type
                '("with_clause"
                  "subunit"
                  "entry_declaration")))))

(defun ada-ts-mode--statement-p (node)
  "Determine if NODE is a statement."
  (or (ada-ts-mode--simple-statement-p node)
      (ada-ts-mode--compound-statement-p node)))

(defun ada-ts-mode--simple-statement-p (node)
  "Determine if NODE is a simple statement."
  (let ((node-type (treesit-node-type node)))
    (member node-type
            '("null_statement"
              "assignment_statement"
              "exit_statement"
              "goto_statement"
              "procedure_call_statement"
              "simple_return_statement"
              "requeue_statement"

              ;; delay_statement
              "delay_until_statement"
              "delay_relative_statement"

              "abort_statement"
              "raise_statement"
              "pragma_g"))))

(defun ada-ts-mode--compound-statement-p (node)
  "Determine if NODE is a compound statement."
  (let ((node-type (treesit-node-type node)))
    (member node-type
            '("if_statement"
              "gnatprep_if_statement"
              "case_statement"
              "loop_statement"
              "block_statement"
              "extended_return_statement"
              "accept_statement"

              ;; select_statement
              "selective_accept"
              "timed_entry_call"
              "conditional_entry_call"
              "asynchronous_select"))))

(defun ada-ts-mode--declarative-item-p (node)
  "Determine if NODE is a declarative item."
  (or (ada-ts-mode--basic-declarative-item-p node)
      (let ((node-type (treesit-node-type node)))
        (member node-type
                '(;; proper_body
                  "subprogram_body"
                  "package_body"
                  "task_body"
                  "protected_body"

                  "body_stub")))))

(defun ada-ts-mode--basic-declarative-item-p (node)
  "Determine if NODE is a basic declarative item."
  (or (ada-ts-mode--basic-declaration-p node)
      (let ((node-type (treesit-node-type node)))
        (member node-type
                '(;; aspect_clause
                  "attribute_definition_clause"
                  "enumeration_representation_clause"
                  "record_representation_clause"
                  "at_clause"

                  "use_clause")))))

(defun ada-ts-mode--basic-declaration-p (node)
  "Determine if NODE is a basic declaration."
  (let ((node-type (treesit-node-type node)))
    (member node-type
            '(;; type_declaration
              "full_type_declaration"
              "incomplete_type_declaration"
              "private_type_declaration"
              "private_extension_declaration"

              "subtype_declaration"
              "object_declaration"
              "number_declaration"
              "subprogram_declaration"
              "expression_function_declaration"
              "null_procedure_declaration"
              "package_declaration"

              ;; renaming_declaration
              "object_renaming_declaration"
              "exception_renaming_declaration"
              "package_renaming_declaration"
              "subprogram_renaming_declaration"
              "generic_renaming_declaration"

              "exception_declaration"

              ;; generic_declaration
              "generic_subprogram_declaration"
              "generic_package_declaration"

              "generic_instantiation"))))

;;; Electric Indentation

(defconst ada-ts-indent--electric-punctuation
  '(";" ")" "]" "=>", ",")
  "Ada punctuation which should trigger electric indentation.

The specified punctuation is only considered if it is entered at the end
of the line.")

(defconst ada-ts-indent--electric-keywords
  '("begin" "else" "#else" "elsif" "#elsif" "end" "#end" "exception" "or" "private" "then" "when")
  "Ada keywords which should trigger electric indentation.

The specified keywords are only considered if they are the only thing on
the line.")

(defun ada-ts-indent--electric-indent-p (&optional _char)
  "Determine if electric indentation should be performed.

When triggered by `self-insert-command', CHAR will be the character
inserted, else nil."
  (when-let* (((not (bobp)))
              (node (treesit-node-at (1- (point))))
              (end (treesit-node-end node))
              ((= end (point)))
              (start (treesit-node-start node))
              (type (treesit-node-type node)))
    (or (member type ada-ts-indent--electric-punctuation)
        (and (= start
                (save-excursion
                  (back-to-indentation)
                  (point)))
             (or (member type ada-ts-indent--electric-keywords)
                 ;; Re-indent identifier that looked like a keyword
                 ;; (and was likely indented as a keyword) before last
                 ;; key press.
                 (and (string-equal type "identifier")
                      (let* ((text (treesit-node-text node 'no-property))
                             (text- (substring text 0 (1- (length text)))))
                        (member-ignore-case
                         text-
                         ada-ts-indent--electric-keywords))))))))

(defvar-local ada-ts-indent--electric-indent-check-needed nil)

(defun ada-ts-indent--maybe-electric-indent ()
  "Maybe perform electric indentation."
  (when ada-ts-indent--electric-indent-check-needed
    (when (and (or (null ada-ts-indent--last-indent-tick)
                   (not (= ada-ts-indent--last-indent-tick
                           (buffer-chars-modified-tick))))
               (ada-ts-indent--electric-indent-p))
      (ignore-errors (indent-according-to-mode)))
    (setq ada-ts-indent--electric-indent-check-needed nil)))

(defun ada-ts-indent--after-change (beg end length)
  "Buffer local after-change function.

Only check indentation on text insertion (i.e., LENGTH = 0) or text
replacement (BEG /= END), but ignore changes that are deletions
only (BEG = END and LENGTH /= 0) as the user typically does not want to
cause electric indentation through deletion of characters immediately
following electric punctuation or electric keywords."
  (when (and (or (zerop length)
                 (/= beg end))
             (bound-and-true-p electric-indent-mode)
             (not (bound-and-true-p electric-indent-inhibit)))
    (setq ada-ts-indent--electric-indent-check-needed t)))

(defun ada-ts-indent--setup ()
  "Setup indentation for buffer."
  (setq-local treesit-simple-indent-rules ada-ts-mode--indent-rules)

  ;; When `electric-indent-mode' is enabled, it only performs electric
  ;; condition checks after `self-insert-command'.  There are other
  ;; commands which can modify the buffer (e.g., `completion-at-point'),
  ;; but no check is performed by `electric-indent-mode'.
  ;;
  ;; As a workaround, hook into `after-change-functions' to know when
  ;; the buffer has changed.  Also hook into `post-command-hook' to
  ;; perform the electric condition check when `electric-indent-mode' is
  ;; enabled and we've detected a buffer change that wasn't due to
  ;; indentation.
  (add-hook 'after-change-functions #'ada-ts-indent--after-change nil 'local)
  (add-hook 'post-command-hook #'ada-ts-indent--maybe-electric-indent nil 'local))

(provide 'ada-ts-indentation)

;;; ada-ts-indentation.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("als/"     . "ada-ts-als-")
;;                          ("advice/"  . "ada-ts-mode--advice-")
;;                          ("anchor/"  . "ada-ts-mode--anchor-")
;;                          ("matcher/" . "ada-ts-mode--matcher-")
;;                          ("offset/"  . "ada-ts-mode--offset-"))
;; End:
