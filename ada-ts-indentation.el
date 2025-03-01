;;; ada-ts-indentation.el -- Indentation support in Ada files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Troy Brown

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

(require 'ada-ts-mode-lspclient)
(require 'cl-generic)
(require 'treesit)
(eval-when-compile (require 'rx))

(defvar ada-ts-mode--keywords nil) ; definition in ada-ts-mode.el
(declare-function ada-ts-mode--defun-name   "ada-ts-mode" (node))
(declare-function ada-ts-mode--node-to-name "ada-ts-mode" (node))

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

;;; Formal Indentation Rules

(defvar ada-ts-mode--indent-rules
  `((ada

     ((or (query ((ERROR) @node))
          (query ((ERROR _ @node)))
          (query ((_ (ERROR) _ @node)))
          no-node) ; newline
      (anchor/best-effort)
      (offset/best-effort))

     ;; top-level
     ((query ([(compilation      _ @node)
               (compilation_unit _ @node)]))
      column-0 0)

     ;; with_clause / use_clause
     ((query ([(with_clause [(identifier) (selected_component)]
                            [(identifier) (selected_component) ","] @node)
               (use_clause [(identifier) (selected_component)]
                           [(identifier) (selected_component) ","] @node)]))
      (anchor/first-sibling "identifier" "selected_component")
      0)

     ;; subunit
     ((query ((subunit [(subprogram_body) (package_body) (task_body) (protected_body)] @node))) column-0                   0)
     ((query ((subunit [(identifier) (selected_component)] @node)))                             (anchor/first-sibling "(") 1)

     ;; aspect_mark_list / aspect_association
     ((query ((aspect_specification) @node))      parent-bol ada-ts-mode-indent-broken-offset)
     ((query ((aspect_mark_list) @node))          parent     ada-ts-mode-indent-broken-offset)
     ((query ((aspect_mark_list _ @node)))        parent     0)
     ((query ((aspect_association _ @node "=>"))) parent     0)

     ;; expression
     ((query ([(array_delta_aggregate (expression) @node)
               (record_delta_aggregate (expression) @node)]))
      parent
      1)
     ((query ((expression_function_declaration (expression) @node)))
      (anchor/first-sibling "(")
      1)
     ((query ([(case_expression_alternative  (expression) @node)
               (declare_expression           (expression) @node)
               (if_expression         "then" (expression) @node)
               (elsif_expression_item "then" (expression) @node)]))
      parent
      ada-ts-mode-indent-offset)

     ((query ((expression) @node))   parent ada-ts-mode-indent-broken-offset)
     ((query ((expression _ @node))) parent ada-ts-mode-indent-exp-item-offset)

     ;; discrete_choice_list
     ((query ((discrete_choice_list _ @node))) parent 0)
     ((query ((discrete_choice_list) @node))   parent ada-ts-mode-indent-broken-offset)

     ;; case_statement / case_statement_alternative
     ((query ((case_statement_alternative) @node))          parent ada-ts-mode-indent-when-offset)
     ((query ((case_statement_alternative "=>" _ @node)))   parent ada-ts-mode-indent-offset)
     ((query ((case_statement "is" (comment) @node "end"))) parent ada-ts-mode-indent-when-offset)

     ;; case_expression_alternative
     ((query ((case_expression_alternative) @node)) parent ada-ts-mode-indent-when-offset)

     ;; if_expression / case_expression / declare_expression / quantified_expression
     ((query ([(if_expression)
               (case_expression)
               (declare_expression)
               (quantified_expression)]
              @node))
      (anchor/prev-sibling "(")
      1)

     ;; variant_part / variant_list / component_list / record_definition
     ((query ((component_list _ @node)))          parent 0)
     ((query ((variant_part "is" _ @node "end"))) parent ada-ts-mode-indent-when-offset)
     ((query ((variant_list _ @node)))            parent 0)
     ((query ((variant "=>" _ @node)))            parent ada-ts-mode-indent-offset)

     ;; parameter_specification
     ((query ((_ (parameter_specification) (parameter_specification) @node)))
      (anchor/first-sibling "parameter_specification")
      0)
     ((query ((parameter_specification) @node)) parent-bol ada-ts-mode-indent-broken-offset)

     ;; result_profile
     ((query ((result_profile) @node))   parent-bol ada-ts-mode-indent-broken-offset)
     ((query ((result_profile _ @node))) parent     ada-ts-mode-indent-broken-offset)

     ;; access_definition
     ((query ((access_definition subtype_mark: _ @node))) parent 0)

     ;; parameter_association
     ((query ((_ (parameter_association) (parameter_association) @node)))
      (anchor/first-sibling "parameter_association")
      0)
     ((query ((parameter_association) @node)) parent-bol ada-ts-mode-indent-broken-offset)

     ;; named_array_aggregate / array_delta_aggregate
     ((query ([(named_array_aggregate (array_component_association)
                                      [(array_component_association) ","] @node)
               (array_delta_aggregate (array_component_association)
                                      [(array_component_association) ","] @node)]))
      (anchor/first-sibling "array_component_association")
      0)
     ((query ((named_array_aggregate (array_component_association) @node))) parent 1)
     ((query ((array_delta_aggregate ["with" "delta" (array_component_association)] @node)))
      (anchor/prev-sibling "expression")
      ada-ts-mode-indent-broken-offset)

     ;; record_component_association_list
     ((query ((record_component_association_list _ @node))) parent 0)

     ;; record_delta_aggregate
     ((query ((record_delta_aggregate ["with" "delta" (record_component_association_list)] @node)))
      (anchor/prev-sibling "expression")
      ada-ts-mode-indent-broken-offset)

     ;; enumeration_type_definition
     ((query ((enumeration_type_definition
               [(identifier) (character_literal)]
               [(identifier) (character_literal) ","] @node)))
      (anchor/first-sibling "identifier" "character_literal")
      0)
     ((query ((enumeration_type_definition [(identifier) (character_literal)] @node)))
      (anchor/first-sibling "(")
      1)

     ;; pragma_argument_association
     ((query ((_ (pragma_argument_association)
                 (pragma_argument_association) @node)))
      (anchor/first-sibling "pragma_argument_association")
      0)
     ((query ((pragma_argument_association) @node)) parent ada-ts-mode-indent-broken-offset)


     ;; exception_declaration
     ((query ((exception_declaration _ @node))) parent 0)

     ;; extended_return_object_declaration
     ((query ((extended_return_object_declaration) @node)) parent ada-ts-mode-indent-broken-offset)

     ;; protected_definition
     ((and (query ((protected_definition ["private" "end"] @node)))
           (n-p-gp nil nil "ERROR"))
      (anchor/best-effort)
      (offset/best-effort))
     ((and (query ((protected_definition "end" (identifier) @node)))
           (n-p-gp nil nil "ERROR"))
      (anchor/best-effort)
      (offset/best-effort))
     ((query ((protected_definition :anchor ["private" "end"]) @node)) parent       0)
     ((query ((protected_definition) @node))                           parent       ada-ts-mode-indent-offset)
     ((query ((protected_definition ["private" "end"] @node)))         grand-parent 0)
     ((query ((protected_definition "end" (identifier) @node)))        grand-parent ada-ts-mode-indent-broken-offset)
     ((query ((protected_definition :anchor "private" (_) @node)))     parent       ada-ts-mode-indent-offset)
     ((query ((protected_definition (_) @node)))                       parent       0)

     ;; task_definition
     ((and (query ((task_definition ["private" "end"] @node)))
           (n-p-gp nil nil "ERROR"))
      (anchor/best-effort)
      (offset/best-effort))
     ((and (query ((task_definition "end" (identifier) @node)))
           (n-p-gp nil nil "ERROR"))
      (anchor/best-effort)
      (offset/best-effort))
     ((query ((task_definition :anchor ["private" "end"]) @node)) parent       0)
     ((query ((task_definition) @node))                           parent       ada-ts-mode-indent-offset)
     ((query ((task_definition ["private" "end"] @node)))         grand-parent 0)
     ((query ((task_definition "end" (identifier) @node)))        grand-parent ada-ts-mode-indent-broken-offset)
     ((query ((task_definition :anchor "private" (_) @node)))     parent       ada-ts-mode-indent-offset)
     ((query ((task_definition (_) @node)))                       parent       0)

     ;; generic_instantiation
     ((query ((generic_instantiation generic_name: _ @node))) parent ada-ts-mode-indent-broken-offset)

     ;; discriminant_specification_list / discriminant_specification
     ((query ((_ (discriminant_specification) [(discriminant_specification) ";"] @node)))
      (anchor/first-sibling "discriminant_specification")
      0)
     ((query ((discriminant_specification_list) @node)) (anchor/first-sibling "(") 1)

     ;; null_procedure_declaration / expression_function_declaration / abstract subprogram_declaration
     ((query ([(expression_function_declaration "is" @node)
               (null_procedure_declaration      "is" @node)
               (subprogram_declaration          "is" @node "abstract")]))
      parent-bol
      ada-ts-mode-indent-subprogram-is-offset)
     ((query ([(null_procedure_declaration "null" @node)
               (subprogram_declaration "abstract" @node)]))
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; handled_sequence_of_statements
     ((and (query ((handled_sequence_of_statements "exception" @node)))
           (n-p-gp nil nil "ERROR"))
      (anchor/best-effort)
      (offset/best-effort))
     ((query ((handled_sequence_of_statements "exception" @node)))           (anchor/gp-skip-label-bol)                 0)
     ((query ((handled_sequence_of_statements (exception_handler) _ @node))) (anchor/first-sibling "exception_handler") 0)
     ((query ((handled_sequence_of_statements _ @node)))                     parent-bol                                 0)
     ((query ((exception_handler "=>" _ @node)))                             parent                                     ada-ts-mode-indent-offset)
     ((query ((exception_handler _ @node)))                                  parent                                     ada-ts-mode-indent-broken-offset)
     ((query ((exception_choice_list (exception_choice) _ @node)))           (anchor/first-sibling "exception_choice")  0)

     ;; non_empty_declarative_part
     ((query ((non_empty_declarative_part _ @node))) parent-bol 0)

     ;; prevent keywords from aligning to parent BOL.
     ((query ([(if_expression ["then" "else" (elsif_expression_item)] @node)
               (case_expression "is" @node)
               (declare_expression "begin" @node)]))
      parent
      0)
     ((query ((declare_expression (_) @node "begin"))) parent ada-ts-mode-indent-offset)
     ((query ((quantifier) @node))                     parent ada-ts-mode-indent-broken-offset)

     ;; Handle special record type indentation.
     ((query ([(record_definition) (record_type_definition) (record_representation_clause)] @node))
      (anchor/best-effort)
      (offset/best-effort))
     ((query ([(record_type_definition       _ @node)
               (record_definition            _ @node)
               (record_representation_clause _ @node)]))
      (anchor/best-effort)
      (offset/best-effort))

     ;; loop_statement
     ((query ((loop_statement (loop_label) (iteration_scheme) @node)))       parent                                        ada-ts-mode-indent-label-offset)
     ((query ((loop_statement (iteration_scheme) "loop" _ @node "end")))     (anchor/first-sibling-bol "iteration_scheme") ada-ts-mode-indent-offset)
     ((query ((loop_statement (iteration_scheme) ["loop" "end" ";"] @node))) (anchor/first-sibling-bol "iteration_scheme") 0)
     ((query ((loop_statement (iteration_scheme) (identifier) @node)))       (anchor/first-sibling-bol "iteration_scheme") ada-ts-mode-indent-broken-offset)
     ((query ((loop_statement (loop_label) "loop" @node)))                   parent                                        ada-ts-mode-indent-label-offset)
     ((query ((loop_statement "loop" _ @node "end")))                        (anchor/first-sibling-bol "loop")             ada-ts-mode-indent-offset)
     ((query ((loop_statement "loop" ["loop" "end" ";"] @node)))             (anchor/first-sibling-bol "loop")             0)
     ((query ((loop_statement "loop" (identifier) @node)))                   (anchor/first-sibling-bol "loop")             ada-ts-mode-indent-broken-offset)
     ((query ((loop_parameter_specification) @node))                         parent                                        ada-ts-mode-indent-broken-offset)
     ((query ((loop_parameter_specification _ @node)))                       parent                                        0)
     ((query ((iterator_specification) @node))                               parent                                        ada-ts-mode-indent-broken-offset)
     ((query ((iterator_specification _ @node)))                             parent                                        0)

     ;; block_statement
     ((query ((block_statement (loop_label) "declare" @node)))        parent                               ada-ts-mode-indent-label-offset)
     ((query ((block_statement "declare" ["begin" "end" ";"] @node))) (anchor/first-sibling-bol "declare") 0)
     ((query ((block_statement "declare" _ @node "begin")))           (anchor/first-sibling-bol "declare") ada-ts-mode-indent-offset)
     ((query ((block_statement "declare" "begin" _ @node "end")))     (anchor/first-sibling-bol "declare") ada-ts-mode-indent-offset)
     ((query ((block_statement "declare" (identifier) @node)))        (anchor/first-sibling-bol "declare") ada-ts-mode-indent-broken-offset)
     ((query ((block_statement (loop_label) "begin" @node)))          parent                               ada-ts-mode-indent-label-offset)
     ((query ((block_statement "begin" ["end" ";"] @node)))           (anchor/first-sibling-bol "begin")   0)
     ((query ((block_statement "begin" _ @node "end")))               (anchor/first-sibling-bol "begin")   ada-ts-mode-indent-offset)
     ((query ((block_statement "begin" (identifier) @node)))          (anchor/first-sibling-bol "begin")   ada-ts-mode-indent-broken-offset)

     ;; keywords / semicolon
     ((query ([ ,@ada-ts-mode--keywords ";"
                (elsif_statement_item)
                (aspect_specification)
                (null_exclusion)
                (access_to_object_definition)
                (access_to_subprogram_definition)
                (procedure_specification)
                (function_specification)
                (allocator)]
              @node))
      parent-bol
      0)

     ;; select_statement
     ((query ((_ (guard) :anchor (comment) :* :anchor [(select_alternative) (comment)] @node)))
      (anchor/prev-sibling "guard")
      ada-ts-mode-indent-offset)
     ((query ((selective_accept       _ @node))) parent ada-ts-mode-indent-offset)
     ((query ((timed_entry_call       _ @node))) parent ada-ts-mode-indent-offset)
     ((query ((conditional_entry_call _ @node))) parent ada-ts-mode-indent-offset)
     ((query ((asynchronous_select    _ @node))) parent ada-ts-mode-indent-offset)
     ((query ((accept_alternative     _ @node))) parent 0)
     ((query ((delay_alternative      _ @node))) parent 0)
     ((query ((entry_call_alternative _ @node))) parent 0)
     ((query ((triggering_alternative _ @node))) parent 0)

     ((query ([(subprogram_body           "is"   _ @node "end")
               (package_body              "is"   _ @node "end")
               (package_declaration       "is"   _ @node "end")
               (task_body                 "is"   _ @node "end")
               (entry_body                "is"   _ @node "end")
               (protected_body            "is"   _ @node "end")
               (extended_return_statement "do"   _ @node "end")
               (if_statement              "then" _ @node "end")
               (elsif_statement_item      "then" _ @node)]))
      parent-bol
      ada-ts-mode-indent-offset)

     ;; general indentation for comments.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and (node-is "comment")
           (ada-ts-mode--next-sibling-not-matching-exists-p "comment"))
      (anchor/next-sibling-not-matching "comment")
      (offset/next-sibling-not-matching "comment"))

     ;; identifier / selected_component
     ((query ([(identifier) (selected_component)] @node)) parent-bol ada-ts-mode-indent-broken-offset)
     ((query ((selected_component _ @node)))              parent-bol ada-ts-mode-indent-broken-offset)

     ;; non-expression opening parenthesis
     ((query ([(formal_part)
               (enumeration_aggregate)
               (enumeration_type_definition)
               (actual_parameter_part)
               (known_discriminant_part)
               (unknown_discriminant_part)
               "("]
              @node))
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; closing parenthesis (including expression)
     ((query ((_ "(" ")" @node))) (anchor/first-sibling "(") 0)
     ((query ((_ "[" "]" @node))) (anchor/first-sibling "[") 0)

     ;; miscellaneous punctuation
     ((query ([":" ":="] @node)) parent 0)
     ((query ("=>" @node))       parent ada-ts-mode-indent-broken-offset)

     ;; If rule set is complete, this rule should never be matched.
     (catch-all (anchor/catch-all) (offset/catch-all))))
  "Tree-sitter indent rules for `ada-ts-mode'.")

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

(defun ada-ts-mode--prev-node (start &optional include-comments)
  "Find node before START, and possibly INCLUDE-COMMENTS.

START is either a node or a position."
  (let* ((prev-node-s
          (if (treesit-node-p start)
              (treesit-node-start start)
            start))
         (first-pass t)
         prev-node prev-node-e prev-node-t)
    (save-excursion
      (while (or first-pass
                 (and prev-node-t
                      (not include-comments)
                      (string-equal prev-node-t "comment")))
        (setq first-pass nil)
        (goto-char prev-node-s)
        (skip-chars-backward " \t\n" (point-min))
        (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
              prev-node-e (treesit-node-end prev-node))
        (setq prev-node
              (treesit-parent-while
               prev-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-end node) prev-node-e))))
              prev-node-t (treesit-node-type prev-node)
              prev-node-s (treesit-node-start prev-node))))
    prev-node))

(defun ada-ts-mode--next-node (start)
  "Find node after START.

START is either a node or a position."
  (let* ((next-node-e
          (if (treesit-node-p start)
              (treesit-node-end start)
            (1+ start)))
         (first-pass t)
         next-node next-node-s next-node-t)
    (save-excursion
      (while (or first-pass
                 (and next-node-t
                      (string-equal next-node-t "comment")))
        (setq first-pass nil)
        (goto-char next-node-e)
        (skip-chars-forward " \t\n" (point-max))
        (setq next-node (if (eobp) nil (treesit-node-at (point)))
              next-node-s (treesit-node-start next-node))
        (setq next-node
              (treesit-parent-while
               next-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-start node) next-node-s))))
              next-node-t (treesit-node-type next-node)
              next-node-e (treesit-node-end next-node))))
    next-node))

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

  MATCHES is either a list of strings consisting of node types or a
  predicate function which takes a node as its sole parameter and returns
  non nil for a match."
  (let ((prev-node start)
        (predicate (if (functionp matches)
                       matches
                     (lambda (node)
                       (member (treesit-node-type node) matches)))))
    (while (or (treesit-node-eq prev-node start)
               (and prev-node
                    (not (funcall predicate prev-node))))
      (setq prev-node (ada-ts-mode--prev-node prev-node)))
    prev-node))

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
      (save-excursion
        (goto-char (treesit-node-start anchor-node))
        (back-to-indentation)
        (treesit-node-at (point))))))

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
    ;; Keyword: "begin"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "begin"))))
      (setq scenario "Scenario [Keyword: 'begin']")
      (cond ((string-equal prev-node-t "is")
             (when-let* ((anchor-node (ada-ts-mode--is-keyword-anchor prev-node)))
               (setq anchor (treesit-node-start anchor-node)
                     offset 0)))
            ((string-equal prev-node-t "begin")
             (setq anchor (treesit-node-start prev-node)
                   offset ada-ts-mode-indent-offset))
            (t
             (when-let* ((prev-node (ada-ts-mode--prev-node node))
                         (prev-node-t (treesit-node-type prev-node)))
               (if (string-equal prev-node-t "non_empty_declarative_part")
                   (setq anchor (treesit-node-start prev-node)
                         offset (- ada-ts-mode-indent-offset))
                 (setq anchor (treesit-node-start prev-node)
                       offset 0))))))
    ;; Keyword: "limited"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "limited")))
                (next-node (ada-ts-mode--next-leaf-node node))
                (next-node-t (treesit-node-type next-node))
                ((string-equal next-node-t "record"))
                (anchor-node (ada-ts-mode--matching-prev-node node '("type"))))
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
                (prev-prev-node (ada-ts-mode--prev-node prev-node))
                (prev-prev-node-t (treesit-node-type prev-prev-node))
                ((member prev-prev-node-t '("is" "tagged" "limited" "use" "with")))
                (prev-node-s (treesit-node-start prev-node)))
      ;; Anchor to "limited" or "record" keyword if it's at the
      ;; beginning of the line, otherwise anchor to the "type"
      ;; (in type definition) or "for" (in representation
      ;; clause) keyword, which might be on a different line,
      ;; especially if discriminant arguments are present.
      (let* ((bol-node
              (save-excursion
                (goto-char prev-node-s)
                (back-to-indentation)
                (treesit-node-at (point))))
             (bol-node-t (treesit-node-type bol-node)))
        (if (member bol-node-t '("limited" "record"))
            (setq anchor (treesit-node-start bol-node)
                  offset ada-ts-mode-indent-offset
                  scenario "Scenario [After Keyword: 'record']")
          (when-let* ((anchor-node (ada-ts-mode--matching-prev-node prev-node '("for" "type"))))
            (setq anchor (treesit-node-start anchor-node)
                  offset ada-ts-mode-indent-offset
                  scenario "Scenario [After Keyword: 'record']")))))
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
    ;; After "elsif_statement_item"
    (when-let* (((not anchor))
                ((and node-t
                      (member node-t '("elsif" "elsif_statement_item" "else" "end"))))
                ((string-equal prev-node-t "elsif_statement_item")))
      (setq anchor (treesit-node-start prev-node)
            offset 0
            scenario "Scenario [After 'elsif_statement_item']"))
    ;; Keywords after "statement"
    (when-let* (((not anchor))
                ((and node-t
                      (member node-t '("elsif" "elsif_statement_item" "else" "end" "exception"))))
                ((or (string-equal prev-node-t "handled_sequence_of_statements")
                     (ada-ts-mode--statement-p prev-node))))
      (setq anchor (treesit-node-start prev-node)
            offset (- ada-ts-mode-indent-offset)
            scenario "Scenario [Keywords after 'statement']"))
    ;; Keyword: "then"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "then")))
                (anchor-node (ada-ts-mode--then-keyword-anchor node)))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: 'then']"))
    ;; Keywords after "then"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "then"))
                (anchor-node (ada-ts-mode--then-keyword-anchor prev-node))
                (anchor-node-t (treesit-node-type anchor-node))
                ((member anchor-node-t '("elsif" "#elsif" "if" "#if"))))
      (setq anchor (treesit-node-start anchor-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [Keywords after 'then']"))
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
            ;; Account for possible loop label
            (save-excursion
              (goto-char (treesit-node-start prev-node))
              (back-to-indentation)
              (point))
            offset (if (string-equal prev-node-t "begin")
                       ada-ts-mode-indent-offset
                     0)
            scenario "Scenario [Keyword: 'loop']"))
    ;; After Keyword: "loop"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "loop"))
                (prev-prev-node (ada-ts-mode--prev-node prev-node))
                (prev-prev-node-t (treesit-node-type prev-prev-node)))
      (setq anchor
            ;; Account for possible loop label
            (save-excursion
              (goto-char
               (treesit-node-start
                (if (string-equal prev-prev-node-t "iteration_scheme")
                    prev-prev-node
                  prev-node)))
              (back-to-indentation)
              (point))
            offset ada-ts-mode-indent-offset
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
    ;; Keyword: "end"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "end"))))
      (setq anchor (treesit-node-start prev-node)
            offset (- ada-ts-mode-indent-offset)
            scenario "Scenario [Keyword: 'end']"))
    ;; Keywords: "or", "else" (select statement)
    (when-let* (((not anchor))
                ((and node-t (member node-t '("or" "else"))))
                (prev-token (ada-ts-mode--prev-token node))
                ((string-equal prev-token ";"))
                (anchor-node (ada-ts-mode--matching-prev-node node '("select"))))
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
                (anchor-node (ada-ts-mode--matching-prev-node prev-node '("select"))))
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
    ;; Newline after case_statement_alternative
    (when-let* (((not anchor))
                ((not node))
                ((string-equal prev-node-t "case_statement_alternative")))
      (setq anchor (treesit-node-start prev-node)
            offset ada-ts-mode-indent-offset
            scenario "Scenario [Newline after case_statement_alternative]"))
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
      (setq anchor (treesit-node-start statement-node)
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
    ;; After Keywords: ada-ts-mode-indent-offset
    (when-let* (((not anchor))
                ((member prev-node-t '("begin" "declare" "else" "exception" "generic" "private" "record" "select"))))
      (setq anchor (save-excursion
                     (goto-char (treesit-node-start prev-node))
                     (back-to-indentation)
                     (point))
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
      (setq anchor (save-excursion
                     (goto-char (treesit-node-start prev-node))
                     (back-to-indentation)
                     (point))
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
    (cons anchor offset)))

;;; Indentation Anchors and Offsets

(defun anchor/best-effort ()
  "Determine best-effort anchor."
  (lambda (node parent bol &rest _)
    (let ((anchor (car (ada-ts-mode--indent-best-effort node parent bol))))
      (when ada-ts-mode--indent-verbose
        (message "Anchor: %s" anchor))
      anchor)))

(defun offset/best-effort ()
  "Determine best-effort offset."
  (lambda (node parent bol &rest _)
    (let ((offset (cdr (ada-ts-mode--indent-best-effort node parent bol))))
      (when ada-ts-mode--indent-verbose
        (message "Offset: %s" offset))
      offset)))

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

(defun ada-ts-mode--next-sibling-not-matching (type &rest types)
  "Locate next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let ((all-types (cons type types))
          (sibling-node (treesit-node-next-sibling node)))
      (while (and sibling-node
                  (seq-some (lambda (a-type)
                              (equal (treesit-node-type sibling-node) a-type))
                            all-types))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'ada-ts-mode--next-sibling-not-matching-exists-p
  'ada-ts-mode--next-sibling-not-matching)

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

(defun ada-ts-mode--indent-line ()
  "Perform line indentation."
  (ada-ts-mode-indent-line ada-ts-mode-indent-backend))

(defun ada-ts-mode--indent-region (beg end)
  "Perform region indentation between BEG and END."
  (ada-ts-mode-indent-region ada-ts-mode-indent-backend beg end))

(cl-defgeneric ada-ts-mode-indent-line (backend)
  "Indent line using BACKEND."
  (error "Unknown indentation backend: %s" backend))

(cl-defgeneric ada-ts-mode-indent-region (backend _beg _end)
  "Indent region between BEG and END using BACKEND."
  (error "Unknown indentation backend: %s" backend))

;;;; LSP

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql lsp)))
  "Indent line using LSP server BACKEND.

If an LSP client is not active, or the line to indent is empty, or if
the LSP region formatting function fails, fallback to tree-sitter based
indentation.

The Ada Language Server does not indent empty lines and will fail to
indent when syntax errors exist, therefore the need to fallback on
tree-sitter indentation in these scenarios."
  (if-let* ((client (lspclient/current)))
      (if (save-excursion
            (forward-line 0)
            (looking-at-p (rx (* whitespace) eol)))
          ;; Handle extraneous space as well as implement a workaround
          ;; for LSP onTypeFormatting for RET as described in
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=70929
          (ada-ts-mode-indent-line 'tree-sitter)
        (condition-case _
            (let ((initial-point-column (current-column))
                  (initial-indentation-column (current-indentation)))
              (ada-ts-mode-indent-region 'lsp (pos-bol) (pos-eol))
              ;; For consistency with built-in indentation behavior,
              ;; if point was in the white-space at the beginning of
              ;; the line, move point to the current indentation.
              (when (<= initial-point-column
                        initial-indentation-column)
                (back-to-indentation)))
          (error
           (ada-ts-mode-indent-line 'tree-sitter))))
    ;; fallback on tree-sitter indentation
    (ada-ts-mode-indent-line 'tree-sitter)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql lsp)) beg end)
  "Indent the region between BEG and END using LSP server BACKEND.

When CLIENT is not nil, use it as the active LSP client."
  (if-let* ((client (lspclient/current)))
      (let ((inhibit-message t)
            (tab-width ada-ts-mode-indent-offset)
            (standard-indent ada-ts-mode-indent-offset))
        (lspclient/format-region client beg end))
    ;; fallback on tree-sitter indentation
    (ada-ts-mode-indent-region 'tree-sitter beg end)))

;;;; Tree-sitter

(cl-defgeneric ada-ts-mode-indent (strategy)
  "Indent using tree-sitter back-end, according to STRATEGY."
  (error "Unknown indentation strategy: %s" strategy))

(cl-defmethod ada-ts-mode-indent ((_strategy (eql line)))
  "Indent using tree-sitter back-end, according to line STRATEGY."
  (treesit-indent))

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
                    (root (treesit-buffer-root-node))
                    (candidate
                     (treesit-parent-until
                      node
                      (lambda (node)
                        (or (treesit-node-eq node root)
                            (string-equal (treesit-node-type node) "ERROR")
                            (ada-ts-mode--compilation-unit-p node)))
                      'include-node)))
               (when (and (ada-ts-mode--compilation-unit-p candidate)
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
                         (treesit-node-end candidate)))))))))
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
                   (ada-ts-mode--defun-name node))
                  ((string-equal node-t "block_statement")
                   (let ((first-child-node (treesit-node-child node 0)))
                     (when (string-equal (treesit-node-type first-child-node) "loop_label")
                       (ada-ts-mode--node-to-name
                        (treesit-node-child-by-field-name first-child-node "statement_identifier")))))
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
                          ((string-equal next-node-t "identifier")))
                (ada-ts-mode--node-to-name next-node)))))
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

(provide 'ada-ts-indentation)

;;; ada-ts-indentation.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("lspclient/" . "ada-ts-mode-lspclient-")
;;                          ("advice/"    . "ada-ts-mode--advice-")
;;                          ("anchor/"    . "ada-ts-mode--anchor-")
;;                          ("offset/"    . "ada-ts-mode--offset-"))
;; End:
