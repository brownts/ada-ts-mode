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
(require 'ada-ts-predicates)
(require 'cl-generic)
(require 'treesit)
(eval-when-compile (require 'rx))

(defvar ada-ts-mode--keywords nil) ;; definition in ada-ts-mode.el

(defcustom ada-ts-mode-indent-backend 'treesitter
  "Backend used for indentation."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Tree-sitter" treesitter)
                 (const :tag "Language Server" lsp))
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))
;;;###autoload(put 'ada-ts-mode-indent-backend 'safe-local-variable #'symbolp)

(defcustom ada-ts-mode-indent-strategy 'smart
  "Indentation strategy to utilize with tree-sitter backend."
  :type '(choice :tag "Indentation Strategy"
                 (const :tag "Smart" smart)
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

(defcustom ada-ts-mode-indent-exp-item-offset 0
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

;;; Indentation Error Recovery

(defun ada-ts-mode--indent-error-recovery (op)
  "Find indentation in the presence of syntax errors.

If OP is \\='anchor\\=', determine anchor.  If OP is \\='offset\\=',
determine offset."
  (lambda (node parent bol &rest _)
    (let (
          (node-type
           (let ((type (treesit-node-type node)))
             (when (and type (string-equal type "ERROR"))
               ;; Replace ERROR node with leaf node.
               (setq node (treesit-node-at (treesit-node-start node)))
               (setq type (treesit-node-type node)))
             type))
          prev-node prev-type prev-end parent-node offset)
      (let ((type (treesit-node-type parent))
            prev-parent)
        (message "parent-type: %s" type)
        (message "parent-type children length: %d" (length (treesit-node-children parent)))
        (while (and (string-equal type "ERROR")
                    (<= (length (treesit-node-children parent)) 1))
          (setq prev-parent parent
                parent (treesit-node-parent parent)
                type (treesit-node-type parent))
          (message "while parent-type: %s" type)
          (message "while parent-type children length: %d" (length (treesit-node-children parent)))

          )
        (unless parent
          (setq parent prev-parent)))

      (save-excursion
        (beginning-of-line)
        (skip-chars-backward " \t\n" (point-min))
        (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
              prev-type (treesit-node-type prev-node))
        (while (and prev-node
                    (member prev-type '("comment" "," "." ":" "|" "..")))
          (when (member prev-type '("," "|"))
            ;; Don't indent at all for these.  Helps to
            ;; vertically align lists.
            (setq offset 0))
          (goto-char (treesit-node-start prev-node))
          (skip-chars-backward " \t\n" (point-min))
          (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
                prev-type (treesit-node-type prev-node)))

        (setq prev-end (treesit-node-end prev-node))
        (setq parent-prev-node prev-node)
        (setq parent-prev-node
              (treesit-parent-while
               parent-prev-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-end node) prev-end)))))

        ;; ;; Handle parameter lists gracefully
        ;; (when (and (treesit-node-eq parent-prev-node prev-node)
        ;;            (string-equal prev-type ";"))
        ;;   (while (and prev-node
        ;;               (member (prev-type '("comment" ";"))))
        ;;     (


        ;; NOTE: Better handling of "begin"?  Check parent for
        ;; "function_specification" (or procedure_specification)
        ;; followed by "is".  If these were only specifications, they
        ;; wouldn't be followed by an "is".  If they were
        ;; declarations, they'd be a "declaration" node type (e.g.,
        ;; "expression_function_declaration") since the specification
        ;; is buried at a lower layer.

        ;; begin:  Filter children of "parent" before "node", matching:
        ;;         "is", "begin", "function_specification"
        (let ((indent-specs `(
                              ( :node "formal_part"
                                :pref-node-type "identifier"
                                :filter-nodes ("identifier")
                                :matches (( :match-types ("identifier")
                                            :anchor-index 0 :anchor-bol t :offset ,ada-ts-mode-indent-broken-offset)))

                              ( :node nil ; any node
                                :prev-node-type ";"
                                :filter-nodes ("parameter_specification" ";")
                                :matches (( :match-types ("parameter_specification" ";")
                                            :anchor-index 1 :anchor-bol nil :offset 0)))
                              ( :node nil :prev-node-type ":="
                                :filter-nodes (":=")
                                :matches (( :match-types (":=")
                                            :anchor-index 0 :anchor-bol t :offset ,ada-ts-mode-indent-broken-offset)))
                              ( :node "("
                                :filter-nodes ("function" "procedure" "identifier" "selected_component" "(" "is")
                                :matches (
                                          ( :match-types ("function" "identifier")
                                            :anchor-index 1 :anchor-bol t :offset ,ada-ts-mode-indent-broken-offset)
                                          ( :match-types ("function" "selected_component")
                                            :anchor-index 1 :anchor-bol t :offset ,ada-ts-mode-indent-broken-offset)
                                          ( :match-types ("procedure" "identifier")
                                            :anchor-index 1 :anchor-bol t :offset ,ada-ts-mode-indent-broken-offset)
                                          ( :match-types ("procedure" "selected_component")
                                            :anchor-index 1 :anchor-bol t :offset ,ada-ts-mode-indent-broken-offset)

                                          ))
                              ( :node "begin"
                                :filter-nodes ("function_specification"
                                               "procedure_specification"
                                               "declare"
                                               "is"
                                               "begin")
                                :matches (( :match-types ("function_specification" "is")
                                            :anchor-index 0 :anchor-bol t :offset 0)
                                          ( :match-types ("procedure_specification" "is")
                                            :anchor-index 0 :anchor-bol t :offset 0)
                                          ( :match-types ("declare")
                                            :anchor-index 0 :anchor-bol t :offset 0)))
                              ( :node "exception"
                                :filter-nodes ("function_specification"
                                               "procedure_specification"
                                               "is"
                                               "begin"
                                               "exception")
                                :matches (( :match-types ("function_specification" "is" "begin")
                                            :anchor-index 0 :anchor-bol t :offset 0)
                                          ( :match-types ("procedure_specification" "is" "begin")
                                            :anchor-index 0 :anchor-bol t :offset 0)
                                          ( :match-types ("begin")
                                            :anchor-index 0 :anchor-bol t :offset 0)))
                              ( :node nil
                                :prev-node-type "is"
                                :filter-nodes ("case" "is")
                                :matches (( :match-types ("case" "is")
                                            :anchor-index 1 :anchor-bol nil :offset ,ada-ts-mode-indent-when-offset)))
                              ( :node nil
                                :prev-node-type "=>"
                                :filter-nodes ("when" "=>")
                                :matches (( :match-types ("when" "=>")
                                            :anchor-index 1 :anchor-bol nil :offset ,ada-ts-mode-indent-offset)))
                              ( :node nil
                                :prev-node-type "declare"
                                :filter-nodes ("(" "declare")
                                :matches (( :match-types ("(" "declare")
                                            :anchor-index 0 :anchor-bol nil :offset ,ada-ts-mode-indent-offset)))
                              ( :node nil
                                :prev-node-type "is"
                                :filter-nodes ("function_specification"
                                               "procedure_specification"
                                               "is"
                                               "begin"
                                               "exception")
                                :matches (( :match-types ("function_specification" "is")
                                            :anchor-index 1 :anchor-bol t :offset ,ada-ts-mode-indent-offset)
                                          ( :match-types ("procedure_specification" "is")
                                            :anchor-index 1 :anchor-bol t :offset ,ada-ts-mode-indent-offset)))

                              ;; ( :node "package"
                              ;;   :filter-nodes ("generic_formal_part"
                              ;;                  "generic"
                              ;;                  "with"
                              ;;                  "package")
                              ;;   :matches (( :match-types ("generic_formal_part")
                              ;;               :anchor-index 0 :anchor-bol t :offset 0)))
                              ))
              anchor)
          (unless prev-node
            (message "Scenario 1")
            (setq anchor (point-min))
            (setq offset 0))
          (unless anchor
            (while (and (not anchor)
                        indent-specs)
              (when-let* ((indent-spec (car indent-specs))
                          ((let ((spec-node-type (plist-get indent-spec :node)))
                             (cond ((null spec-node-type) t) ; Don't require node match
                                   ((null node-type) nil)
                                   (t  (string-equal node-type spec-node-type)))))
                          ((let ((spec-prev-node-type (plist-get indent-spec :prev-node-type)))
                             (cond ((null spec-prev-node-type) t) ; Don't require prev-node match
                                   (t (string-equal (treesit-node-type prev-node) spec-prev-node-type)))))
                          ((progn (message "Got here: 1") t))
                          (filter-nodes (plist-get indent-spec :filter-nodes))
                          ((progn (message "Got here: 2") t))
                          (children-reversed
                           (list
                            (reverse
                             (treesit-filter-child
                              parent
                              (lambda (n)
                                (and (< (treesit-node-start n) bol)
                                     (member (treesit-node-type n) filter-nodes)))))
                            (reverse
                             (treesit-filter-child
                              (treesit-node-parent prev-node)
                              (lambda (n)
                                (and (< (treesit-node-start n) bol)
                                     (member (treesit-node-type n) filter-nodes)))))))
                          ((progn (message "Got here: 4") t))
                          (children-type-reversed
                           (list
                            (mapcar #'treesit-node-type (car children-reversed))
                            (mapcar #'treesit-node-type (cadr children-reversed))))
                          (matches (plist-get indent-spec :matches)))
                (message "children-type-reversed-0.1: %s" (car children-type-reversed))
                (message "children-type-reversed-0.2: %s" (cadr children-type-reversed))
                (while (and (not anchor)
                            matches)
                  (when-let* ((match (car matches))
                              (match-types-reversed (reverse (plist-get match :match-types)))
                              ((setq children-reversed
                                     (cond ((equal match-types-reversed
                                                   (seq-take (car children-type-reversed)
                                                             (length match-types-reversed)))
                                            (car children-reversed))
                                           ((equal match-types-reversed
                                                   (seq-take (cadr children-type-reversed)
                                                             (length match-types-reversed)))
                                            (cadr children-reversed))))))
                    (message "Scenario 1.5")
                    (message "match-types-reversed: %s" match-types-reversed)
                    (message "node: %s" node)
                    (message "parent: %s" parent)
                    ;; (message "  children: %s" (treesit-node-children parent))
                    (message "prev-node: %s" prev-node)
                    ;; (message "  children: %s" (treesit-node-children (treesit-node-parent prev-node)))
                    (message "parent-prev-node: %s" parent-prev-node)
                    (setq anchor (treesit-node-start
                                  (seq-elt children-reversed
                                           (plist-get match :anchor-index)))
                          offset (plist-get match :offset))
                    (when (plist-get match :anchor-bol)
                      (goto-char anchor)
                      (back-to-indentation)
                      (setq anchor (point))))
                  (setq matches (cdr matches))))
              (setq indent-specs (cdr indent-specs))))
          (when (and (not anchor)
                     (member prev-type '("begin" "declare" "exception" "generic" "loop" "private" "record" "is")))
            (message "Scenario 2")
            (message "node: %s" node)
            (message "parent: %s" parent)
            (message "  children: %s" (treesit-node-children parent))
            (message "prev-node: %s" prev-node)
            (message "  children: %s" (treesit-node-children (treesit-node-parent prev-node)))
            (message "parent-prev-node: %s" parent-prev-node)
            (setq anchor (progn (back-to-indentation)
                                (point))
                  offset ada-ts-mode-indent-offset))
          (when (and (not anchor)
                     (member prev-type ada-ts-mode--keywords))
            (message "Scenario 2.25")
            (message "node: %s" node)
            (message "parent: %s" parent)
            (message "  children: %s" (treesit-node-children parent))
            (message "prev-node: %s" prev-node)
            (message "  children: %s" (treesit-node-children (treesit-node-parent prev-node)))
            (message "parent-prev-node: %s" parent-prev-node)
            (setq anchor (progn (back-to-indentation)
                                (point))
                  offset (or offset ada-ts-mode-indent-broken-offset)))
          (when (and (not anchor)
                     (not (treesit-node-eq parent-prev-node prev-node))
                     (member (treesit-node-type parent-prev-node) '("generic_formal_part")))
            (message "Scenario 2.5")
            ;; Handles 2nd and more generic formal parameters
            (setq anchor (treesit-node-start parent-prev-node)
                  offset ada-ts-mode-indent-offset))
          ;; (when (and (not anchor)
          ;;            (not (treesit-node-eq parent-prev-node prev-node))
          ;;            (member (treesit-node-type parent-prev-node) '("loop_label")))
          ;;   (message "Scenario 2.75")
          ;;   (setq anchor (treesit-node-start parent-prev-node)
          ;;         offset 0))
          (when (and (not anchor)
                     (string-equal prev-type "("))
            (message "Scenario 3")
            (setq anchor (treesit-node-start prev-node)
                  offset 1))
          (when (and (not anchor)
                     (not (treesit-node-eq parent-prev-node prev-node)))
            (message "Scenario 4")
            (message "node: %s" node)
            (message "parent: %s" parent)
            (message "  children: %s" (treesit-node-children parent))
            (message "prev-node: %s" prev-node)
            (message "  children: %s" (treesit-node-children (treesit-node-parent prev-node)))
            (message "parent-prev-node: %s" parent-prev-node)
            (message "op: %s" op)
            (setq anchor (treesit-node-start parent-prev-node)
                  offset 0))
          (unless anchor
            (message "Scenario 5")
            (message "node: %s" node)
            (message "parent: %s" parent)
            (message "prev-node: %s" prev-node)
            (message "parent-prev-node: %s" parent-prev-node)
            (setq anchor (treesit-node-start prev-node)
                  offset (or offset ada-ts-mode-indent-broken-offset)))
          (cond ((eq op 'anchor) anchor)
                ((eq op 'offset) offset)
                (t (error "Unknown op: %s" op))))))))

(defun ada-ts-mode--anchor-of-indent-error-recovery ()
  "Determine indentation anchor of error recovery point."
  (ada-ts-mode--indent-error-recovery 'anchor))

(defun ada-ts-mode--offset-of-indent-error-recovery ()
  "Determine indentation offset of error recovery point."
  (ada-ts-mode--indent-error-recovery 'offset))

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
    (if-let* ((sibling-node
               (ada-ts-mode--first-child-matching parent sibling)))
        (< (treesit-node-start sibling-node) bol))))

(defun ada-ts-mode--before-first-sibling-p (sibling)
  "Determine if the location of node comes before SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
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
    (when-let* ((sibling-node (ada-ts-mode--first-child-matching parent sibling)))
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

(defun ada-ts-mode--prev-sibling (node parent bol &rest _)
  "Determine previous sibling in PARENT before this NODE or BOL."
  (if node
      (treesit-node-prev-sibling node)
    (car
     (reverse
      (treesit-filter-child
       parent
       (lambda (n)
         (< (treesit-node-start n) bol)))))))

(defun ada-ts-mode--prev-sibling-matches-p (type)
  "Check if previous sibling matches TYPE."
  (lambda (node parent bol &rest _)
    (if-let* ((prev (ada-ts-mode--prev-sibling node parent bol)))
        (string-equal (treesit-node-type prev) type))))

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

     ;; Error recover rules.

     ((or (parent-is "ERROR")
          (node-is "ERROR")
          (ada-ts-mode--prev-sibling-matches-p "ERROR"))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))

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

     (catch-all
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))

     ))
  "Tree-sitter indent rules for `ada-ts-mode'.")

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
            (looking-at-p (rx (* whitespace) eol) t))
          ;; Handle extraneous space as well as implement a workaround
          ;; for LSP onTypeFormatting for RET as described in
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=70929
          (ada-ts-mode-indent-line 'treesitter)
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
           (ada-ts-mode-indent-line 'treesitter))))
    ;; fallback on tree-sitter indentation
    (ada-ts-mode-indent-line 'treesitter)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql lsp)) beg end)
  "Indent the region between BEG and END using LSP server BACKEND.

When CLIENT is not nil, use it as the active LSP client."
  (if-let* ((client (lspclient/current)))
      (let ((inhibit-message t)
            (tab-width ada-ts-mode-indent-offset)
            (standard-indent ada-ts-mode-indent-offset))
        (lspclient/format-region client beg end))
    ;; fallback on tree-sitter indentation
    (ada-ts-mode-indent-region 'treesitter beg end)))

;;;; Default

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql default)))
  "Indent line using default BACKEND."
  (if-let* ((indent-line (default-value 'indent-line-function)))
      (funcall indent-line)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql default)) beg end)
  "Indent the region between BEG and END using default BACKEND."
  (if-let* ((indent-region (default-value 'indent-region-function)))
      (funcall indent-region beg end)))

;;;; Tree-sitter

(cl-defgeneric ada-ts-mode-indent (strategy)
  "Indent using tree-sitter back-end, according to STRATEGY."
  (error "Unknown indentation strategy: %s" strategy))

(cl-defmethod ada-ts-mode-indent ((_strategy (eql line)))
  "Indent using tree-sitter back-end, according to line STRATEGY."
  (treesit-indent))

(cl-defmethod ada-ts-mode-indent ((_strategy (eql smart)))
  "Indent using tree-sitter back-end, according to smart STRATEGY."
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
                   (when treesit--indent-verbose
                     (message "Smart indent triggered for: %s" candidate))
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

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql treesitter)))
  "Indent line using tree-sitter BACKEND."
  (if (eq ada-ts-mode-indent-backend 'treesitter)
      ;; Only utilize indentation strategy when tree-sitter back-end
      ;; is configured, not when tree-sitter back-end is used as a
      ;; fallback, to avoid competing indentation styles.
      (ada-ts-mode-indent ada-ts-mode-indent-strategy)
    (ada-ts-mode-indent 'line)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql treesitter)) beg end)
  "Indent the region between BEG and END using tree-sitter BACKEND."
  (treesit-indent-region beg end))

(defun ada-ts-mode--mismatched-names-p (node)
  "Determine if NODE names are mismatched."
  (when-let* ((node-type (treesit-node-type node))
              ((member node-type
                       '("entry_body"
                         "package_body"
                         "package_declaration"
                         "protected_body"
                         "protected_type_declaration"
                         "single_protected_declaration"
                         "single_task_declaration"
                         "subprogram_body"
                         "task_body"
                         "task_type_declaration")))
              (name (ada-ts-mode--defun-name node)))
    (let (endname-node)
      (pcase node-type
        ((or "package_body"
             "package_declaration"
             "subprogram_body")
         (setq endname-node (treesit-node-child-by-field-name node "endname"))))
      (when endname-node
        (not (string-equal name (ada-ts-mode--node-to-name endname-node)))))))

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
;; read-symbol-shorthands: (("lspclient/" . "ada-ts-mode-lspclient-"))
;; End:
